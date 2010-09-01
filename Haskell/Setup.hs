{-# LANGUAGE NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import qualified Distribution.PackageDescription as D
import Distribution.Verbosity
import System.Directory
import System.Process
import Maybe
import qualified Distribution.ModuleName as Modname
main = do
  includeDir <- readProcess "ruby" ["-rrbconfig", "-e", "print RbConfig::CONFIG['rubyhdrdir']"] ""
  archDir <- readProcess "ruby" ["-rrbconfig", "-e", "print RbConfig::CONFIG['archdir'].gsub(/\\/lib\\//, '/include/').gsub(/\\/include\\/ruby\\//, '/include/ruby-')"] ""
  defaultMainWithHooks (hooks includeDir archDir)

hooks includeDir archDir = simpleUserHooks
  {
   preConf = \arg flags -> do
      -- probably a nicer way of getting that directory...
      createDirectoryIfMissing True "dist/build/autogen"
      -- FILTHY HACK
      writeFile "dist/build/autogen/Includes.hs" ("module Includes where\nextraIncludeDirs=[\"" ++ includeDir++"\",\"" ++ archDir ++ "\"]") -- show (configExtraIncludeDirs flags))
      return D.emptyHookedBuildInfo,
   confHook = \ info flags ->  (confHook simpleUserHooks) info (flags { configSharedLib = Flag True, configExtraIncludeDirs = [includeDir] }),
   sDistHook = \ pkg lbi hooks flags -> let lib = fromJust $ D.library pkg
                                            modules = filter (/= Modname.fromString "Includes") $ D.exposedModules lib
                                            pkg' = pkg { D.library = Just $ lib { D.exposedModules = modules } }   
                                        in sDistHook simpleUserHooks pkg' lbi hooks flags  

  }
