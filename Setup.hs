{-# LANGUAGE NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Verbosity
import System.Directory
main = defaultMainWithHooks hooks

hooks = simpleUserHooks
  {
   preConf = \arg flags -> do
      -- probably a nicer way of getting that directory...
      createDirectoryIfMissing True "dist/build/autogen"
      writeFile "dist/build/autogen/Includes.hs" ("module Includes where\nextraIncludeDirs=" ++ show (configExtraIncludeDirs flags))
      return emptyHookedBuildInfo
  }