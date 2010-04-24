{-# LANGUAGE NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import qualified Distribution.PackageDescription as D
import Distribution.Verbosity
import System.Directory
import System.Process

main = do
  includeDir <- readProcess "ruby" ["-rrbconfig", "-e", "print RbConfig::CONFIG['archdir']"] ""
  defaultMainWithHooks (hooks includeDir)

hooks includeDir = simpleUserHooks
  {
   preConf = \arg flags -> do
      -- probably a nicer way of getting that directory...
      createDirectoryIfMissing True "dist/build/autogen"
      -- FILTHY HACK
      writeFile "dist/build/autogen/Includes.hs" ("module Includes where\nextraIncludeDirs=[\"" ++ includeDir++"\"]") -- show (configExtraIncludeDirs flags))
      return D.emptyHookedBuildInfo,
   confHook = \ info flags ->  (confHook simpleUserHooks) info (flags { configSharedLib = Flag True, configExtraIncludeDirs = [includeDir] })

  }