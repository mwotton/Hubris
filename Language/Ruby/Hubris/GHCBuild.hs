module Language.Ruby.Hubris.GHCBuild (ghcBuild, defaultGHCOptions, GHCOptions(..)) where
import Config
import Debug.Trace
import DynFlags
import GHC
import GHC.Paths
import Outputable
import StringBuffer
import System.Time
import Control.Monad(forM_)

newtype GHCOptions = GHCOptions { strict :: Bool }
defaultGHCOptions = GHCOptions { strict = True }
type Filename = String


sh = showSDoc . ppr
-- this one's a bit tricky: we _could_ use the GHC api, but i don't care too much.
-- let's keep it simple.
--
-- ok, new plan: handling it the filthy way is awful.
ghcBuild :: Filename -> String -> String -> [Filename] -> GHCOptions -> IO Bool
ghcBuild libFile immediateSource modName extra_sources options =
       do srcBuffer <- stringToStringBuffer immediateSource
          putStrLn ("modname is " ++ modName)
          writeFile "/tmp/foo.hs" immediateSource
          time <- getClockTime -- i suppose we've just made it now...
          defaultErrorHandler defaultDynFlags $ do
          res <- runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags

            (newflags, leftovers, warnings) <- GHC.parseDynamicFlags dflags 
                                               $ map noLoc $ (words $ "-shared -o " ++ libFile ++ " -optl-Wl,-rpath," ++ libdir 
                                                      ++ " /Users/mwotton/projects/Hubris/lib/rshim.o -lHSrts-ghc" ++ Config.cProjectVersion)
            trace ("left: " ++ sh leftovers) $ trace (sh warnings) $ setSessionDynFlags newflags
            
            forM_ ("/tmp/foo.hs":extra_sources)  (\file -> guessTarget file Nothing >>= addTarget) 

            load LoadAllTargets
            -- do something with c_sources and extra_sources
--             setTargets [Target { targetContents = Just ( srcBuffer, time ), 
--                                  targetAllowObjCode = True,
--                                  targetId = TargetModule $ mkModuleName modName
--                                } ]

                              -- } ]
       
          return (case res of
                  Succeeded -> True
                  _ -> False)

