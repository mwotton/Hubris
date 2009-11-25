module Main where
import Language.Ruby.Hubris.LibraryBuilder
import System
import System.Exit
import Language.Ruby.Hubris.ZCode (zenc,zdec) 

main = do
   (mod:extra_src) <- getArgs
   -- HACK this may be the worst thing ever
   let src = filter (\x -> x /= "--strict") extra_src
       args = if (length src == length extra_src)
              then []
              else [] -- ["-Wall", "-Werror"]
   res <- generateLib ("/var/hubris/cache/" ++ zenc mod ++ ".bundle")  src mod args
   print res
   case res of
     Left a  -> putStrLn ("Failed: " ++ a) >> exitFailure
     Right _ -> exitSuccess
