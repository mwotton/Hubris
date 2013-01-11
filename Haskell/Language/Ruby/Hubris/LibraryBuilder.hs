
{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
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

import GHC(parseStaticFlags, noLoc)


import Language.Ruby.Hubris.ZCode (zenc,Zname(..))

type Filename = String
dotrace a b = b

-- weirdly, mapMaybeM doesn't exist.
mapMaybeM :: (Functor m, Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM func ls  = catMaybes <$> (sequence $ map func ls)

generateLib :: Filename -> [Filename] -> ModuleName -> [String] -> [String] -> IO (Either Filename String)
generateLib libFile sources moduleName buildArgs packages = do
  -- set up the static args once  
  GHC.parseStaticFlags $ map noLoc $ map ("-package "++) ("hubris":packages)

  s <- generateSource sources moduleName
  
  either (return . Left . show)
         (\(c,mod) -> do bindings <- withTempFile "hubris_interface_XXXXX.c" c
                         ghcBuild libFile mod ("Language.Ruby.Hubris.Exports." ++ moduleName) sources ([bindings] ++ buildArgs))
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
        rubyVal = "(fromIntegral $ fromEnum $ Language.Ruby.Hubris.Binding.RUBY_Qtrue)"
        haskellVal = "(Language.Ruby.Hubris.toHaskell " ++ rubyVal ++ ")"
        genApp qualName i = unwords (qualName:(take i $ repeat haskellVal))
                                     
generateSource :: [Filename] ->   -- optional haskell source to load into the interpreter
                   ModuleName ->   -- name of the module to build a wrapper for
                   IO (Either InterpreterError (String,String))
generateSource sources moduleName = runInterpreter $ do
         loadModules sources
         setImportsQ $ [(mod,Just mod) | mod <- ["Language.Ruby.Hubris","Language.Ruby.Hubris.Binding", moduleName]]
         funcs <- getFunctions moduleName 
         say ("Candidates: " ++ show funcs)
         mapM (exportable moduleName) funcs >>= \x -> say (show x)
         exports :: [(Funcname, Int,  Wrapper)] <- mapMaybeM (exportable moduleName) funcs
         say ("Exportable: " ++ show exports)
         -- return (undefined, undefined)
         return (genC [(a,b) | (a,b,_) <- exports] (zenc moduleName),
                 unlines (haskellBoilerplate moduleName:[wrapper | (_,_,wrapper) <- exports]))
                          
getFunctions moduleName = (\ x -> [a |Fun a <- x]) <$> getModuleExports moduleName


genC :: [(String,Int)] -> Zname -> String
genC exports (Zname zmoduleName) = unlines $ 
         ["#include <stdio.h>"
          ,"#include <stdlib.h>"
          ,"#define HAVE_STRUCT_TIMESPEC 1"
          ,"#include <ruby.h>"
--          ,"#define DEBUG 1"
          ,"#ifdef DEBUG"
          ,"#define eprintf printf"
          ,"#else"
          ,"int eprintf(const char *f, ...){}"
          ,"#endif"
         ] ++
--         map (("VALUE hubrish_"++) . (++"(VALUE);")) exports ++
--         map (("VALUE hubrish_"++) . (++"(VALUE);")) exports ++
         map cWrapper exports ++
         ["extern void safe_hs_init();"
         ,"extern VALUE Exports;"
         ,"void Init_" ++ zmoduleName ++ "(){"
         ,"  eprintf(\"loading " ++ zmoduleName ++ "\\n\");"
         ,"  VALUE Fake = Qnil;"
         ,"  safe_hs_init();"
         ,"  Fake = rb_define_module_under(Exports, \"" ++ zmoduleName ++ "\");"
         ] ++ map cDef exports ++  ["}"]
  where
    cWrapper :: (String,Int) -> String
    cWrapper (f,arity) = 
      let res = unlines 
                ["VALUE hubrish_" ++ f ++ "("++ (concat . intersperse "," . take arity $ repeat "VALUE") ++ ");",
                 "VALUE " ++ f ++ "(VALUE mod, VALUE v){"
                ,"  eprintf(\""++f++" has been called\\n\");"
                 -- also needs to curry on the ruby side
                 
                 -- v is actually an array now, so we need to stash each element in
                 -- a nested haskell tuple. for the moment, let's just take the first one.
                               
                ,"  unsigned long res = hubrish_" ++ f ++ "(" ++ intercalate "," ["rb_ary_entry(v," ++ show i ++ ")"| i<- [0..(arity-1)]] ++ ");"
                ,"  eprintf(\"hubrish "++f++" has been called\\n\");"
                ,"  eprintf(\"result is %p\\n\",res);"
                 --                                         ,"  res = res | 0x100000000;"
                ,"  if (rb_obj_is_kind_of(res,rb_eException)) {"
                ,"    eprintf(\""++f++" has provoked an exception\\n\");"                               
                ,"    rb_exc_raise(res);"
                ,"  } else {"
                ,"    eprintf(\"returning from "++f++"\\n\");"
                ,"    return res;"
                ,"  }"
                ,"}"]
      in res 

    cDef :: (String,Int) -> String
    -- adef f =  "  eprintf(\"Defining |" ++ f  ++ "|\\n\");\n" ++ "rb_define_method(Fake, \"" ++ f ++"\","++ f++", 1);"
    cDef (f,_arity) =  "  eprintf(\"Defining |" ++ f  ++ "|\\n\");\n" ++ "rb_define_method(Fake, \"" ++ f ++"\","++ f++", -2);"

haskellBoilerplate moduleName = unlines ["{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}", 
                                         "module Language.Ruby.Hubris.Exports." ++ moduleName ++ " where",
                                         "import Language.Ruby.Hubris",
                                         "import Language.Ruby.Hubris.Binding",
                                         "import System.IO.Unsafe (unsafePerformIO)",
                                         "import Control.Monad",
                                         "import Control.Exception",
                                         "import Data.Either",
                                         "import Data.Function(($))",
                                         "import qualified Prelude as P(show,putStrLn)",
                                         "import Data.Tuple (uncurry)",
                                         "import Foreign.C.Types",
                                         "import qualified " ++ moduleName]



genWrapper (func,arity) mod = unlines $ [func ++ " :: " ++ myType
                                            ,func ++ " " ++  unwords symbolArgs ++ " = " ++ defHask 
                                            ,"foreign export ccall \"hubrish_" ++  func ++ "\" " ++ func ++ " :: " ++ myType]
  where myType = intercalate "->" (take (1+arity) $ repeat " CULong ")
        -- mark's patented gensyms. just awful.
        symbolArgs = take arity $ map ( \ x -> "fake_arg_symbol_"++[x]) ['a' .. 'z']
        defHask = "unsafePerformIO $ do\n  r <- try $ evaluate $ toRuby $" ++ mod ++"."++ func  ++ " " ++ unwords (map (\ x -> "(toHaskell " ++ x ++ ")") symbolArgs) ++ "\n  case r of\n" ++     
--                  unlines ["   Left (e::SomeException) -> createException (P.show e) `traces` (\"died in haskell wrapper\" P.++ P.show e) ",
                  unlines ["   Left (e::SomeException) ->  createException (P.show e)" ,
                           "   Right a -> return a"]
 
say :: String -> InterpreterT IO ()
-- say = liftIO . putStrLn
say _ = return ()

-- Local Variables:
-- compile-command: "cd ../../../; ./Setup build"
-- End:
