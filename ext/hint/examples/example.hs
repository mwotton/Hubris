import Control.Monad
import Language.Haskell.Interpreter

main :: IO ()
main = do r <- runInterpreter testHint
          case r of
            Left err -> printInterpreterError err
            Right () -> putStrLn "that's all folks"

-- observe that Interpreter () is an alias for InterpreterT IO ()
testHint :: Interpreter ()
testHint =
    do
      say "Load SomeModule.hs"
      loadModules ["SomeModule.hs"]
      --
      say "Put the Prelude, Data.Map and *SomeModule in scope"
      say "Data.Map is qualified as M!"
      setTopLevelModules ["SomeModule"]
      setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]
      --
      say "Now we can query the type of an expression"
      let expr1 = "M.singleton (f, g, h, 42)"
      say $ "e.g. typeOf " ++ expr1
      say =<< typeOf expr1
      --
      say $ "Observe that f, g and h are defined in SomeModule.hs, " ++
            "but f is not exported. Let's check it..."
      exports <- getModuleExports "SomeModule"
      say (show exports)
      --
      say "We can also evaluate an expression; the result will be a string"
      let expr2 = "length $ concat [[f,g],[h]]"
      say $ concat ["e.g. eval ", show expr1]
      a <- eval expr2
      say (show a)
      --
      say "Or we can interpret it as a proper, say, int value!"
      a_int <- interpret expr2 (as :: Int)
      say (show a_int)
      --
      say "This works for any monomorphic type, even for function types"
      let expr3 = "\\(Just x) -> succ x"
      say $ "e.g. we interpret " ++ expr3 ++
            " with type Maybe Int -> Int and apply it on Just 7"
      fun <- interpret expr3 (as :: Maybe Int -> Int)
      say . show $ fun (Just 7)
      --
      say "And sometimes we can even use the type system to infer the expected type (eg Maybe Bool -> Bool)!"
      bool_val <- (interpret expr3 infer `ap` (return $ Just False))
      say (show $ not bool_val)
      --
      say "Here we evaluate an expression of type string, that when evaluated (again) leads to a string"
      res <- interpret "head $ map show [\"Worked!\", \"Didn't work\"]" infer >>= flip interpret infer
      say res


say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)

