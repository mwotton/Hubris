import Language.Ruby.Hubris.LibraryBuilder

main = do 
  -- Hubris.hs ought to be installed on the system, really.
  source <- generateSource ["Language/Ruby/Hubris.hs","Language/Ruby/Foo.hs"] "Foo"
  case source of
    Left err ->      error $ show err
    Right Nothing -> error "shouldn't happen" -- maybe throw an error in the interpreter monad instead
    Right (Just a) -> putStrLn a