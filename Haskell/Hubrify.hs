module Main where
import Language.Ruby.Hubris.LibraryBuilder
import System.Environment
import System.Exit
import Control.Monad (guard)
import System.Console.GetOpt

data Options = Options
     { optVerbose     :: Bool
     , optStrict      :: Bool
     , optShowVersion :: Bool
     , optOutput      :: FilePath
     , optModule      :: String
     , optInput       :: Maybe FilePath
     , optPackages    :: [String]
     } deriving Show

defaultOptions :: Options
defaultOptions    = Options
     { optVerbose     = False
     , optShowVersion = False
     , optOutput      = error "output must be defined"
     , optModule      = error "module must be defined"
     , optStrict      = False
     , optInput       = Nothing
     , optPackages    = []
     }

options :: [OptDescr (Options -> Options)]
options =
     [ Option "v"     ["verbose"]
         (NoArg (\opts -> opts { optVerbose = True }))
         "chatty output on stderr"
     , Option [] ["strict"]
         (NoArg (\opts -> opts { optStrict = True }))
         "bondage and discipline mode"
     , Option "o"     ["output"]
         (ReqArg (\f opts -> opts { optOutput = f }) "libFile")
         "output FILE"
     , Option "m"     ["module"]
         (ReqArg (\f opts -> opts { optModule = f }) "module")
         "module to be wrapped"
     , Option "p"     ["package"]
         (ReqArg (\d opts -> opts { optPackages = optPackages opts ++ [d] }) "DIR")
         "package"
     ]

hubrisOpts :: [String] -> IO (Options, [String])
hubrisOpts argv =
       case getOpt Permute options argv of
          (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
          (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: Hubrify --module MODULE --output LIBFILE (--packages PACKAGE1 ...) sourceFiles ..."

main :: IO ()
main = do
   (o, srcs) <- getArgs >>= hubrisOpts
                      
   let ghcArgs = guard (optStrict o) >> ["-Wall", "-Werror", "-fno-warn-unused-imports"]

   res <- generateLib (optOutput o) srcs (optModule o) ("-fPIC":ghcArgs) (optPackages o)

   either (putStrLn . ("Failed: " ++) >> const exitFailure) (const exitSuccess) res
