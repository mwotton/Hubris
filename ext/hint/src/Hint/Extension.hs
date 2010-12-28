-- this module was automatically generated. do not edit!
module Hint.Extension (Extension(..), knownExtensions)

where

-- | This represents language extensions beyond Haskell 98      that are supported by GHC (it was taken from      Cabal's @Language.Haskell.Extension@)
data Extension = OverlappingInstances
               | UndecidableInstances
               | IncoherentInstances
               | RecursiveDo
               | ParallelListComp
               | MultiParamTypeClasses
               | NoMonomorphismRestriction
               | FunctionalDependencies
               | Rank2Types
               | RankNTypes
               | PolymorphicComponents
               | ExistentialQuantification
               | ScopedTypeVariables
               | ImplicitParams
               | FlexibleContexts
               | FlexibleInstances
               | EmptyDataDecls
               | CPP
               | KindSignatures
               | BangPatterns
               | TypeSynonymInstances
               | TemplateHaskell
               | ForeignFunctionInterface
               | Arrows
               | Generics
               | NoImplicitPrelude
               | NamedFieldPuns
               | PatternGuards
               | GeneralizedNewtypeDeriving
               | ExtensibleRecords
               | RestrictedTypeSynonyms
               | HereDocuments
               | MagicHash
               | TypeFamilies
               | StandaloneDeriving
               | UnicodeSyntax
               | PatternSignatures
               | UnliftedFFITypes
               | LiberalTypeSynonyms
               | TypeOperators
               | RecordWildCards
               | RecordPuns
               | DisambiguateRecordFields
               | OverloadedStrings
               | GADTs
               | NoMonoPatBinds
               | RelaxedPolyRec
               | ExtendedDefaultRules
               | UnboxedTuples
               | DeriveDataTypeable
               | ConstrainedClassMethods
               | PackageImports
               | ImpredicativeTypes
               | NewQualifiedOperators
               | PostfixOperators
               | QuasiQuotes
               | TransformListComp
               | ViewPatterns
               | UnknownExtension String
        deriving (Eq, Show, Read)

knownExtensions :: [Extension]
knownExtensions = [OverlappingInstances,
                   UndecidableInstances,
                   IncoherentInstances,
                   RecursiveDo,
                   ParallelListComp,
                   MultiParamTypeClasses,
                   NoMonomorphismRestriction,
                   FunctionalDependencies,
                   Rank2Types,
                   RankNTypes,
                   PolymorphicComponents,
                   ExistentialQuantification,
                   ScopedTypeVariables,
                   ImplicitParams,
                   FlexibleContexts,
                   FlexibleInstances,
                   EmptyDataDecls,
                   CPP,
                   KindSignatures,
                   BangPatterns,
                   TypeSynonymInstances,
                   TemplateHaskell,
                   ForeignFunctionInterface,
                   Arrows,
                   Generics,
                   NoImplicitPrelude,
                   NamedFieldPuns,
                   PatternGuards,
                   GeneralizedNewtypeDeriving,
                   ExtensibleRecords,
                   RestrictedTypeSynonyms,
                   HereDocuments,
                   MagicHash,
                   TypeFamilies,
                   StandaloneDeriving,
                   UnicodeSyntax,
                   PatternSignatures,
                   UnliftedFFITypes,
                   LiberalTypeSynonyms,
                   TypeOperators,
                   RecordWildCards,
                   RecordPuns,
                   DisambiguateRecordFields,
                   OverloadedStrings,
                   GADTs,
                   NoMonoPatBinds,
                   RelaxedPolyRec,
                   ExtendedDefaultRules,
                   UnboxedTuples,
                   DeriveDataTypeable,
                   ConstrainedClassMethods,
                   PackageImports,
                   ImpredicativeTypes,
                   NewQualifiedOperators,
                   PostfixOperators,
                   QuasiQuotes,
                   TransformListComp,
                   ViewPatterns
                   ]
