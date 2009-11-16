{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Ruby.Hubris.LibraryBuilder (generateLib) where
import Language.Ruby.Hubris.ZCode (zenc, zdec)
import Language.Ruby.Hubris
import Language.Haskell.Interpreter
import Language.Haskell.Meta.QQ.HsHere
import Language.Ruby.Hubris.GHCBuild

import Debug.Trace
import Control.Monad
import Control.Monad.Error.Class

import GHC(parseStaticFlags, noLoc)
type Filename = String

generateLib :: [Filename] -> ModuleName -> IO (Maybe Filename)
generateLib sources moduleName = do
  -- set up the static args once
  GHC.parseStaticFlags $ map noLoc $ words "-dynamic -fPIC -package hubris"

  let libFile = zenc ("libHubris_" ++ moduleName)
  s <- generateSource sources moduleName
  case s of
    Left s -> putStrLn ("HINT error: " ++ show s) >> return Nothing
    Right mod -> do
       putStrLn mod
       res <- ghcBuild libFile mod ("Language.Ruby.Hubris.Exports." ++ moduleName) sources defaultGHCOptions 
       
       return (if res 
                   then Just libFile
                   else Nothing)

generateSource :: [Filename] ->   -- optional haskell source to load into the interpreter
                   ModuleName ->   -- name of the module to build a wrapper for
                   IO (Either InterpreterError String)
generateSource sources moduleName = runInterpreter $ do
         say $ show sources
         loadModules sources
         say "loaded"
         -- setTopLevelModules [moduleName]
         setImportsQ $ map (\x->(x,Just x)) $ ("Language.Ruby.Hubris"):("Language.Ruby.Hubris.Binding"):moduleName:[]


         functions <- getFunctions moduleName
         say $ "Candidates: " ++ (show functions)
       -- ok, let's see if we can come up with an expression of the right type


         exportable <- filterM (\func -> do let rubyVal ="(fromIntegral $ fromEnum $ Language.Ruby.Hubris.Binding.RUBY_Qtrue)"
                                            let f = "Language.Ruby.Hubris.wrap " ++ moduleName ++"." ++func ++" " ++ rubyVal
                                            say f
                                            (typeOf f >>= \n -> say $ "type of wrap." ++ func ++ " is " ++ show n) 
                                                 `catchError` (say . show)
                                            typeChecks (f ++ "==" ++ rubyVal )) functions

         say $ "Exportable: " ++ (show exportable)
            -- withTypes <- mapM  (\x ->  typeOf x >>= \t -> return (x,t)) exportable
         return $ unlines $
         -- ideally, all this stuff would be using something like the Interpolator module,
        -- but haskell-src-meta is not going on 6.12 yet, so let's do it traditionally instead.

          ["{-# LANGUAGE ForeignFunctionInterface #-}", 
           "module Language.Ruby.Hubris.Exports." ++ moduleName ++ " where",
          "import Language.Ruby.Hubris",
          "import qualified Prelude",
          "import Language.Ruby.Hubris.Binding",
          "import qualified " ++ moduleName] ++
          [fun ++ " :: Value -> Value" | fun <- exportable ] ++
          [fun ++ " = Language.Ruby.Hubris.wrap " ++ moduleName ++ "." ++  fun | fun <- exportable ] ++

          ["foreign export ccall \"" ++  fun ++ "\" " ++ fun ++ " :: Value -> Value" | fun <- exportable ]

say = liftIO . putStrLn

getFunctions moduleName = do
  exports <- getModuleExports moduleName
  return $ map (\(Fun f) -> f) $ filter isFun exports

isFun (Fun f) = True
isFun _ = False
