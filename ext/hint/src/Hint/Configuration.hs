module Hint.Configuration (

      setGhcOption, setGhcOptions,

      defaultConf, fromConf, onConf,

      get, set, Option, OptionVal(..),

      languageExtensions, availableExtensions, glasgowExtensions, Extension(..),
      installedModulesInScope,

      setUseLanguageExtensions,

      setInstalledModsAreInScopeQualified,
      searchPath
      ) where

import Control.Monad.Error
import Data.Char
import Data.List ( intersect, intercalate )

import qualified Hint.GHC as GHC
import qualified Hint.Compat as Compat
import Hint.Base
import Hint.Util ( partition )

import Hint.Extension

setGhcOptions :: MonadInterpreter m => [String] -> m ()
setGhcOptions opts =
    do old_flags <- runGhc GHC.getSessionDynFlags
       (new_flags,not_parsed) <- runGhc2 Compat.parseDynamicFlags old_flags opts
       when (not . null $ not_parsed) $
            throwError $ UnknownError (concat ["flag: '", unwords opts,
                                               "' not recognized"])
       _ <- runGhc1 GHC.setSessionDynFlags new_flags
       return ()

setGhcOption :: MonadInterpreter m => String -> m ()
setGhcOption opt = setGhcOptions [opt]

defaultConf :: InterpreterConfiguration
defaultConf = Conf {
                language_exts     = [],
                all_mods_in_scope = False,
                search_path       = ["."]
              }


-- | Available options are:
--
--    * 'languageExtensions'
--
--    * 'installedModulesInScope'
data Option m a = Option{_set :: MonadInterpreter m => a -> m (),
                         _get :: MonadInterpreter m => m a}

data OptionVal m = forall a . (Option m a) := a

-- | Use this function to set or modify the value of any option. It is
--   invoked like this:
--
--   @set [opt1 := val1, opt2 := val2,... optk := valk]@
set :: MonadInterpreter m => [OptionVal m] -> m ()
set = mapM_ $ \(opt := val) -> _set opt val

-- | Retrieves the value of an option.
get :: MonadInterpreter m => Option m a -> m a
get = _get

-- | Language extensions in use by the interpreter.
--
-- Default is: @[]@ (i.e. none, pure Haskell 98)
languageExtensions :: MonadInterpreter m => Option m [Extension]
languageExtensions = Option setter getter
    where setter es = do setGhcOptions $ map (mkFlag False) availableExtensions
                         setGhcOptions $ map (mkFlag True)  es
                         onConf $ \c -> c{language_exts = es}
          --
          getter = fromConf language_exts
          --
          mkFlag b (UnknownExtension o) = "-X" ++ concat ["No"|not b] ++ o
          mkFlag b o
            | ('N':'o':c:_) <- show o,
              isUpper c                 = if b
                                            then "-X" ++ show o
                                            else "-X" ++ (drop 2 $ show o)
            | otherwise                 = "-X" ++ concat ["No"|not b] ++ show o

-- | List of the extensions known by the interpreter.
availableExtensions :: [Extension]
availableExtensions = asExtensionList Compat.supportedLanguages

asExtensionList :: [String] -> [Extension]
asExtensionList exts = map read knownPos                  ++
                       map read (map ("No" ++) knownNegs) ++
                       map UnknownExtension unknown
    where (knownPos, unknownPos) = partition isKnown exts
          (knownNegs,unknown)    = partition (isKnown . ("No" ++)) unknownPos
          isKnown e = e `elem` map show knownExtensions


-- | List of extensions turned on when the @-fglasgow-exts@ flag is used
glasgowExtensions :: [Extension]
glasgowExtensions = intersect availableExtensions exts612 -- works also for 608 and 610
    where exts612 = asExtensionList ["PrintExplicitForalls",
                                     "ForeignFunctionInterface",
                                     "UnliftedFFITypes",
                                     "GADTs",
                                     "ImplicitParams",
                                     "ScopedTypeVariables",
                                     "UnboxedTuples",
                                     "TypeSynonymInstances",
                                     "StandaloneDeriving",
                                     "DeriveDataTypeable",
                                     "FlexibleContexts",
                                     "FlexibleInstances",
                                     "ConstrainedClassMethods",
                                     "MultiParamTypeClasses",
                                     "FunctionalDependencies",
                                     "MagicHash",
                                     "PolymorphicComponents",
                                     "ExistentialQuantification",
                                     "UnicodeSyntax",
                                     "PostfixOperators",
                                     "PatternGuards",
                                     "LiberalTypeSynonyms",
                                     "ExplicitForAll",
                                     "RankNTypes",
                                     "ImpredicativeTypes",
                                     "TypeOperators",
                                     "RecursiveDo",
                                     "DoRec",
                                     "ParallelListComp",
                                     "EmptyDataDecls",
                                     "KindSignatures",
                                     "GeneralizedNewtypeDeriving",
                                     "TypeFamilies" ]

-- | When set to @True@, every module in every available package is implicitly
--   imported qualified. This is very convenient for interactive
--   evaluation, but can be a problem in sandboxed environments
--   (e.g. 'System.Unsafe.unsafePerformIO' is in scope).
--
--   Default value is @True@.
--
--   Observe that due to limitations in the GHC-API, when set to @False@, the
--   private symbols in interpreted modules will not be in scope.
installedModulesInScope :: MonadInterpreter m => Option m Bool
installedModulesInScope = Option setter getter
    where getter = fromConf all_mods_in_scope
          setter b = do onConf $ \c -> c{all_mods_in_scope = b}
                        when ( ghcVersion >= 610 ) $
                            setGhcOption $ "-f"                   ++
                                           concat ["no-" | not b] ++
                                           "implicit-import-qualified"

-- | The search path for source files. Observe that every time it is set,
--   it overrides the previous search path. The default is @[\".\"]@.
--
--   Keep in mind that by a limitation in ghc, @\".\"@ is always in scope.
searchPath :: MonadInterpreter m => Option m [FilePath]
searchPath = Option setter getter
    where getter = fromConf search_path
          setter p = do onConf $ \c -> c{search_path = p}
                        setGhcOption $ "-i" -- clear the old path
                        setGhcOption $ "-i" ++ intercalate ":" p


fromConf :: MonadInterpreter m => (InterpreterConfiguration -> a) -> m a
fromConf f = fromState (f . configuration)

onConf :: MonadInterpreter m
       => (InterpreterConfiguration -> InterpreterConfiguration)
       -> m ()
onConf f = onState $ \st -> st{configuration = f (configuration st)}

{-# DEPRECATED setUseLanguageExtensions "Use set [languageExtensions := (ExtendedDefaultRules:glasgowExtensions)] instead." #-}
setUseLanguageExtensions :: MonadInterpreter m => Bool -> m ()
setUseLanguageExtensions False = set [languageExtensions := []]
setUseLanguageExtensions True  = set [languageExtensions := exts]
    where exts = ExtendedDefaultRules : glasgowExtensions

{-# DEPRECATED setInstalledModsAreInScopeQualified "Use set [installedModulesInScope := b] instead." #-}
setInstalledModsAreInScopeQualified :: MonadInterpreter m => Bool -> m ()
setInstalledModsAreInScopeQualified b = set [installedModulesInScope := b]
