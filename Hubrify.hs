module Main where
import Language.Ruby.Hubris.LibraryBuilder
import System

main = do
   (mod:extra_src) <- getArgs
   generateLib extra_src mod >>= print