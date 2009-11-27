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
  GHC.parseStaticFlags $ map noLoc $ words "-dynamic -fPIC -package hubris -package pcre-light" -- urgh, this needs work

  -- let libFile = zenc ("libHubris_" ++ moduleName))
  s <- generateSource sources moduleName
  case s of
    Left s -> return $ Left ("HINT error: " ++ show s)
    Right Nothing -> return $ Left("no functions found!")
    Right (Just (c,mod)) -> do
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
                   IO (Either InterpreterError (Maybe (String,String)))
generateSource sources moduleName = runInterpreter $ do
         let zmoduleName = zenc moduleName
         loadModules sources
         setImportsQ $ map (\x->(x,Just x)) $ ("Language.Ruby.Hubris"):("Language.Ruby.Hubris.Binding"):moduleName:[]
         functions <- getFunctions moduleName
         say $ "Candidates: " ++ (show functions)
       -- ok, let's see if we can come up with an expression of the right type


         exportable  <- filterM (\func -> do let rubyVal ="(fromIntegral $ fromEnum $ Language.Ruby.Hubris.Binding.RUBY_Qtrue)"
                                             let f = "Language.Ruby.Hubris.wrap " ++ moduleName ++"." ++func ++" " ++ rubyVal
                                             -- say f
                                             --    (typeOf f >>= \n -> say $ "type of wrap." ++ func ++ " is " ++ show n) 
                                             --         `catchError` (say . show)
                                             typeChecks (f ++ "==" ++ rubyVal )) functions

         say $ "Exportable: " ++ (show exportable)
         return $ guard (not $ null exportable) >> return (genC exportable zmoduleName ,genHaskell exportable moduleName )                      
                          
genC :: [String] -> String -> String
genC exportable zmoduleName= unlines $ 
         ["#include <stdio.h>"
          ,"#include <stdlib.h>"
          ,"#define HAVE_STRUCT_TIMESPEC 1"
          ,"#include <ruby.h>"
          ,"#ifdef DEBUG"
          ,"#define eprintf printf"
          ,"#else"
          ,"int eprintf(const char *f, ...){}"
          ,"#endif"
         ] ++
         map forwardDecl exportable ++
         map wrapper exportable ++
         ["extern void safe_hs_init();"
         ,"extern VALUE Exports;"
         ,"void Init_" ++ zmoduleName ++ "(){"
         ,"  eprintf(\"loading\\n\");"
         ,"  VALUE Fake = Qnil;"
         ,"  safe_hs_init();"
         ,"  Fake = rb_define_module_under(Exports, \"" ++ zmoduleName ++ "\");"
         ,"  eprintf(\"defined module " ++ rubyName ++ ":%p\\n\", Fake);"
         ] ++ map def exportable ++  ["}"]
  where rubyName = "Hubris::Exports::" ++ zmoduleName

genHaskell exportable moduleName = unlines $ 
                             ["{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}", 
                              "module Language.Ruby.Hubris.Exports." ++ moduleName ++ " where",
                              "import Language.Ruby.Hubris",
                              "import qualified Prelude as P()",
                              "import Language.Ruby.Hubris.Binding",
                              "import qualified " ++ moduleName] ++
                             [fun ++ " :: Value -> Value" | fun <- exportable ] ++

                             [fun ++ " b = (Language.Ruby.Hubris.wrap " ++ moduleName ++ "." ++  fun ++ ") b"| fun <- exportable ] ++ 
                             ["foreign export ccall \"hubrish_" ++  fun ++ "\" " ++ fun ++ " :: Value -> Value" | fun <- exportable ]

say = liftIO . putStrLn

wrapper :: String -> String
wrapper f = let res = unlines ["VALUE " ++ f ++ "(VALUE mod, VALUE v){"
                              ,"  eprintf(\""++f++" has been called\\n\");"
                              ,"  VALUE res = hubrish_" ++ f ++"(v);"
                              ,"  if (rb_obj_is_kind_of(res,rb_eException)) {"
                              ,"    rb_exc_raise(res);"
                              ,"  } else {"
                              ,"    return res;"
                              ,"  }"
                              ,"}"]
            in res 
                                    

def :: String -> String
def f =  "  eprintf(\"Defining |" ++ f  ++ "|\\n\");\n" ++ "rb_define_method(Fake, \"" ++ f ++"\","++ f++", 1);"

forwardDecl::String->String
forwardDecl f =  "VALUE hubrish_" ++ f ++ "(VALUE);"

getFunctions moduleName = do
  exports <- getModuleExports moduleName
  return $ map (\(Fun f) -> f) $ filter isFun exports

isFun (Fun f) = True
isFun _ = False
