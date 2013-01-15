
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.Ruby.Hubris.LibraryBuilder where
import Language.Ruby.Hubris
import Language.Haskell.Interpreter
-- import Language.Haskell.Meta.QQ.HsHere
import Language.Ruby.Hubris.GHCBuild
import Language.Ruby.Hubris.FileUtil 

import Data.List(intercalate,intersperse)
import qualified Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Data.Maybe(catMaybes,fromJust, isJust)
import Text.InterpolatedString.Perl6(qq)
import GHC(parseStaticFlags, noLoc)


import Language.Ruby.Hubris.ZCode (zenc,Zname(..))

type Filename = String
dotrace a b = b

-- weirdly, mapMaybeM doesn't exist.
mapMaybeM :: (Functor m, Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM func ls  = catMaybes <$> mapM func ls

generateLib :: Filename -> [Filename] -> ModuleName -> [String] -> [String] -> IO (Either Filename String)
generateLib libFile sources moduleName buildArgs packages = do
  -- set up the static args once  
  GHC.parseStaticFlags $ map (noLoc . ("-package "++)) ("hubris":packages)

  s <- generateSource sources moduleName
  
  either (return . Left . show)
         (\(c,mod) -> do bindings <- withTempFile "hubris_interface_XXXXX.c" c
                         ghcBuild libFile mod ("Language.Ruby.Hubris.Exports." ++ moduleName) sources (bindings:buildArgs))
         s

                                            
type Funcname = String               
type Wrapper = String


callable ::String -> InterpreterT IO (Maybe Int)
callable func = do
  ok <- typeChecks str
  if not ok
     then return Nothing
     else Just <$> interpret str (as::Int)

  where str = "Language.Ruby.Hubris.arity " ++  parens func 
  

-- ok, let's see if we can come up with an expression of the right type
exportable ::  String -> String -> InterpreterT IO (Maybe (Funcname, Int, Wrapper))
exportable moduleName func = do say $ "checking " ++ qualName
                                -- here's the problem - i want callable to return Maybe, not bomb
                                -- all the way back out to the outer runInterpreter
                                match <- callable qualName
                                case match of
                                  Nothing -> return Nothing
                                  Just i -> do
                                    let wrapped = genApp qualName i
                                    let eqn = wrapped ++ " == " ++ haskellVal
                                    say ("to check: " ++ eqn)
                                    checked <- typeChecks eqn
                                    say ("Succeeded? " ++ show checked)
                                    return $ guard checked>> return (func, i, genWrapper (func,i) moduleName)
                                               
  where qualName = moduleName ++ "." ++ func
        haskellVal = "(Language.Ruby.Hubris.toHaskell (fromIntegral $ fromEnum $ Language.Ruby.Hubris.Binding.RUBY_Qtrue))"
        genApp qualName i = unwords (qualName:replicate i haskellVal)
                                     
generateSource :: [Filename] ->   -- optional haskell source to load into the interpreter
                   ModuleName ->   -- name of the module to build a wrapper for
                   IO (Either InterpreterError (String,String))
generateSource sources moduleName = runInterpreter $ do
         loadModules sources
         setImportsQ [(mod,Just mod) | mod <- ["Language.Ruby.Hubris","Language.Ruby.Hubris.Binding", moduleName]]
         funcs <- getFunctions moduleName 
         say ("Candidates: " ++ show funcs)
         mapM (exportable moduleName) funcs >>= say . show
         exports :: [(Funcname, Int,  Wrapper)] <- mapMaybeM (exportable moduleName) funcs
         say ("Exportable: " ++ show exports)
         -- return (undefined, undefined)
         return (genC [(a,b) | (a,b,_) <- exports] (zenc moduleName),
                 unlines (haskellBoilerplate moduleName:[wrapper | (_,_,wrapper) <- exports]))
                          
getFunctions moduleName = (\ x -> [a |Fun a <- x]) <$> getModuleExports moduleName


genC :: [(String,Int)] -> Zname -> String
genC exports (Zname zmoduleName) = [qq|
#include <stdio.h>
#include <stdlib.h>
#define HAVE_STRUCT_TIMESPEC 1
#include <ruby.h>
#ifdef DEBUG
#define eprintf printf
#else
int eprintf(const char *f, ...)\{\}
#endif
{concatMap cWrapper exports}
extern void safe_hs_init();
extern VALUE Exports;
void Init_{zmoduleName}()\{
  eprintf("loading $zmoduleName\n");
  VALUE Fake = Qnil;
  safe_hs_init();
  Fake = rb_define_module_under(Exports, "$zmoduleName");
  {concatMap cDef exports}
\}
|]
  where
    cWrapper :: (String,Int) -> String
    cWrapper (f,arity) = [qq|
VALUE hubrish_{f}({intercalate "," . take arity $ repeat "VALUE"});
VALUE {f}(VALUE mod, VALUE v)\{
  eprintf("{f} has been called\\n");
  unsigned long res = hubrish_$f({intercalate "," ["rb_ary_entry(v," ++ show i ++ ")"| i<- [0..(arity-1)]]});
  eprintf("hubrish $f has been called\\n");
  eprintf("result is %p\\n",res);

  if (rb_obj_is_kind_of(res,rb_eException)) \{
    eprintf("$f has provoked an exception\\n");
    rb_exc_raise(res);
  \} else \{
    eprintf("returning from $f\\n");
    return res;
  \}
\}
|]

    cDef :: (String,Int) -> String
    cDef (f,_arity) = [qq|
eprintf("Defining |$f|\\n");
rb_define_method(Fake, "$f", $f, -2);|]


haskellBoilerplate :: String -> String
haskellBoilerplate moduleName = [qq|
\{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-\}
module Language.Ruby.Hubris.Exports.$moduleName where
import Language.Ruby.Hubris
import Language.Ruby.Hubris.Binding
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Control.Exception
import Data.Either
import Data.Function(($))
import qualified Prelude as P(show,putStrLn)
import Data.Tuple (uncurry)
import Foreign.C.Types
import qualified $moduleName
|]


genWrapper :: (String, Int) -> String -> String
genWrapper (func,arity) mod = unlines  [func ++ " :: " ++ myType
                                            ,func ++ " " ++  unwords symbolArgs ++ " = " ++ defHask 
                                            ,"foreign export ccall \"hubrish_" ++  func ++ "\" " ++ func ++ " :: " ++ myType]
  where myType = intercalate "->" (replicate (1+arity) " CULong ")
        -- mark's patented gensyms. just awful.
        symbolArgs = take arity $ map ( \ x -> "fake_arg_symbol_"++[x]) ['a' .. 'z']
        defHask = [qq|unsafePerformIO $ do
  r <- try $ evaluate $ toRuby $ $mod.$func { unwords (map (\\x -> "(toHaskell " ++ x ++ ")") symbolArgs) } 
  case r of
    Left (e::SomeException) ->  createException (P.show e)
    Right a -> return a
|]
 
say :: String -> InterpreterT IO ()
-- say = liftIO . putStrLn
say _ = return ()

-- Local Variables:
-- compile-command: "cd ../../../; ./Setup build"
-- End:
