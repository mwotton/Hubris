module Language.Ruby.Hubris.FileUtil where
import System.IO(hPutStr, hClose, openTempFile)
import System.Process( readProcessWithExitCode )
import System.Exit(ExitCode)
withTempFile :: String -> String -> IO String
withTempFile pattern code = do (name, handle) <- openTempFile "/tmp" pattern
                               hPutStr handle code
                               hClose handle
                               return name

noisySystem :: String -> [String] -> IO (ExitCode, String,String)
noisySystem cmd args = (putStrLn . unwords) (cmd:args) >> readProcessWithExitCode cmd args ""
