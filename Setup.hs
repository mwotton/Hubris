{-# LANGUAGE NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Verbosity

main = defaultMainWithHooks hooks

hooks = simpleUserHooks
  {
   preConf = \arg flags -> do
              -- let ConfigFlags{scratch} = undefined
      writeFile "includes.hs" ("module Includes where\nextraIncludeDirs=" ++ show (configExtraIncludeDirs flags))
      return emptyHookedBuildInfo
  }