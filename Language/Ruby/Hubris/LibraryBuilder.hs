{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Ruby.Hubris.LibraryBuilder (generateLib) where
import Language.Ruby.Hubris.ZCode (zenc, zdec)
import Language.Ruby.Hubris
import Language.Haskell.Interpreter
import Language.Haskell.Meta.QQ.HsHere
import Language.Ruby.Hubris.GHCBuild

import List(intersperse)
import Debug.Trace
import Control.Monad
import Control.Monad.Error.Class

import GHC(parseStaticFlags, noLoc)
import System.IO(hPutStr, hClose, openTempFile)
import System.Exit
import Language.Ruby.Hubris.ZCode (zenc,zdec)

type Filename = String


-- argh, this is ugly. should be a withTempFile construct of some kind.
genCFile :: String -> IO String
genCFile code = do (name, handle) <- openTempFile "/tmp" "hubris_interface_XXXXX.c"
                   hPutStr handle code
                   hClose handle
                   return name

generateLib :: Filename -> [Filename] -> ModuleName -> [String] -> IO (Either Filename String)
generateLib libFile sources moduleName buildArgs = do
  -- set up the static args once
  GHC.parseStaticFlags $ map noLoc $ words "-dynamic -fPIC -package hubris"

  -- let libFile = zenc ("libHubris_" ++ moduleName))
  s <- generateSource sources moduleName
  case s of
    Left s -> return $ Left ("HINT error: " ++ show s)
    Right (c,mod) -> do
       putStrLn mod
       putStrLn "C:"
       putStrLn c

       bindings <- genCFile c -- should really delete afterwards.
       res <- ghcBuild libFile mod ("Language.Ruby.Hubris.Exports." ++ moduleName) sources [bindings] buildArgs
       
       return (case res of
                 Nothing -> Right libFile
                 Just (code,str) -> Left $ "code: " ++ show code ++"\nerr:" ++ str)

generateSource :: [Filename] ->   -- optional haskell source to load into the interpreter
                   ModuleName ->   -- name of the module to build a wrapper for
                   IO (Either InterpreterError (String,String))
generateSource sources moduleName = runInterpreter $ do
         let zmoduleName = zenc moduleName
         say $ show sources
         loadModules sources
         say "loaded"
         -- setTopLevelModules [moduleName]
         setImportsQ $ map (\x->(x,Just x)) $ ("Language.Ruby.Hubris"):("Language.Ruby.Hubris.Binding"):moduleName:[]


         functions <- getFunctions moduleName
         say $ "Candidates: " ++ (show functions)
       -- ok, let's see if we can come up with an expression of the right type


         exportable  <- filterM (\func -> do let rubyVal ="(fromIntegral $ fromEnum $ Language.Ruby.Hubris.Binding.RUBY_Qtrue)"
                                             let f = "Language.Ruby.Hubris.wrap " ++ moduleName ++"." ++func ++" " ++ rubyVal
                                             say f
                                             (typeOf f >>= \n -> say $ "type of wrap." ++ func ++ " is " ++ show n) 
                                                  `catchError` (say . show)
                                             typeChecks (f ++ "==" ++ rubyVal )) functions
         let rubyName = "Hubris::Exports::" ++ zmoduleName
         say $ "Exportable: " ++ (show exportable)
         -- withTypes <- mapM  (\x ->  typeOf x >>= \t -> return (x,t)) exportable
         -- ideally, all this stuff would be using something like the Interpolator module,
         -- but haskell-src-meta is not going on 6.12 yet, so let's do it traditionally instead.
         let hask = ["{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}", 
                     "module Language.Ruby.Hubris.Exports." ++ moduleName ++ " where",
                     "import Language.Ruby.Hubris",
                     "import qualified Prelude as P()",
                     "import Language.Ruby.Hubris.Binding",
--                     "import System.IO",
--                     "import Debug.Trace",
                     "import qualified " ++ moduleName] ++
                    -- really hubris_exports should be a guaranteed safe name - FIXME
        --            ["hubris_exports = Language.Ruby.Hubris.wrap (\\ (_ :: Int) -> \"" ++ (concat $ intersperse "," exportable) ++ "\")",
        --             "foreign export ccall \"exports\" hubris_exports :: Value -> Value" ] ++ 
                    -- gah, this doesn't work - have to load haskell runtime first.
--                     ["dummy = putStrLn \"Loaded\" >> hs_init",
--                      "foreign export ccall \"Init_" ++ moduleName ++ "\" dummy :: IO ()"] ++ 
                    [fun ++ " :: Value -> Value" | fun <- exportable ] ++
--                    [fun ++ " a b = trace  (unwords [show a,show b] ++ \"called " ++ fun ++ "\") (Language.Ruby.Hubris.wrap " ++ moduleName ++ "." ++  fun ++ ") b"| fun <- exportable ] ++ 
                    [fun ++ " b = (Language.Ruby.Hubris.wrap " ++ moduleName ++ "." ++  fun ++ ") b"| fun <- exportable ] ++ 
                    ["foreign export ccall \"hubrish_" ++  fun ++ "\" " ++ fun ++ " :: Value -> Value" | fun <- exportable ]
         let c = ["#include <stdio.h>",
                  "#include <stdlib.h>",
                  "#define HAVE_STRUCT_TIMESPEC 1",
                  "#include <ruby.h>"

                  ] ++
                --  ["char * exports[" ++(show $ length exportable + 1) ++ "] = {" ++ (concat $ intersperse "," $ map (\x -> "\"hubris_" ++ x ++ "\"") exportable) ++ "};"] ++
                  map forwardDecl exportable ++
                  map wrapper exportable ++
                  ["extern void safe_hs_init();"
                  ,"extern VALUE Exports;"
                  ,"void Init_" ++ zmoduleName ++ "(){"
                  ,"  printf(\"loading\\n\");"
                  ,"  VALUE Fake = Qnil;"
                  ,"  safe_hs_init();"
                  ,"  Fake = rb_define_module_under(Exports, \"" ++ zmoduleName ++ "\");"
                  --                  "Fake = rb_define_module(\"" ++ rubyName ++"\");",
                  ,"  printf(\"defined module " ++ rubyName ++ ":%p\\n\", Fake);"
                  ] ++ 
                  map def exportable ++  ["}"]
                      
                      
--                          "char * HubrisExports[" ++(show $ length exportable + 1) ++ "] = {" ++ (concat $ intersperse "," $ map (\x -> "\"" ++ x ++ "\"") exportable) ++ "};",
                          
         return (unlines c,unlines hask)

say = liftIO . putStrLn

wrapper :: String -> String
wrapper f = let res = unlines ["VALUE " ++ f ++ "(VALUE mod, VALUE v){"
--                              ,"  printf(\""++f++" has been called\\n\");"
                              ,"  VALUE res = hubrish_" ++ f ++"(v);"
                              ,"  if (rb_obj_is_kind_of(res,rb_eException)) {"
                              ,"    printf(\"exception\");"
                              ,"    rb_exc_raise(res);"
                              ,"  } else {"
                              ,"    // printf(\"no exception\");"
  --                            ,"    printf(\"%p\\n\", res);"
                              ,"    return res;"
                              ,"  }"
                              ,"}"]
            in res --trace ("trace:" ++ res ++"endtrace") res
                                    

def :: String -> String
def f =  "  printf(\"Defining |" ++ f  ++ "|\\n\");\n" ++ "rb_define_method(Fake, \"" ++ f ++"\","++ f++", 1);"

forwardDecl::String->String
forwardDecl f =  "VALUE hubrish_" ++ f ++ "(VALUE);"

getFunctions moduleName = do
  exports <- getModuleExports moduleName
  return $ map (\(Fun f) -> f) $ filter isFun exports

isFun (Fun f) = True
isFun _ = False
