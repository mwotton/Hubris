-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Interpreter
-- License     :  BSD-style
--
-- Maintainer  :  jcpetruzza@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (GHC API)
--
-- A Haskell interpreter built on top of the GHC API
-----------------------------------------------------------------------------
module Language.Haskell.Interpreter(
    -- * The interpreter monad transformer
     MonadInterpreter(..), InterpreterT, Interpreter,
    -- ** Running the interpreter
     runInterpreter,
    -- ** Interpreter options
     Option, OptionVal((:=)),
     get, set,
     languageExtensions, availableExtensions, glasgowExtensions, Extension(..),
     installedModulesInScope, searchPath,

     setUseLanguageExtensions,
     setInstalledModsAreInScopeQualified,
    -- ** Context handling
     ModuleName, isModuleInterpreted,
     loadModules, getLoadedModules, setTopLevelModules,
     setImports, setImportsQ,
     reset,
    -- ** Module querying
     ModuleElem(..), Id, name, children,
     getModuleExports,
#if __GLASGOW_HASKELL__ >= 611
    -- ** Anotations
    -- | Please note below that annotations are an experimental
    -- feature in GHC HEAD.
    -- In the snippets below we use \'LBRACE\' and \'RBRACE\'
    -- to mean \'{\' and \'}\' respectively. We cannot put the
    -- pragmas inline in the code since GHC scarfs them up.
    getModuleAnnotations, getValAnnotations,
#endif
    -- ** Type inference
     typeOf, typeChecks, kindOf,
    -- ** Evaluation
     interpret, as, infer, eval,
    -- * Error handling
     InterpreterError(..), GhcError(..), MultipleInstancesNotAllowed(..),
    -- * Miscellaneous
     ghcVersion,parens,
     module Control.Monad.Trans)

where

import Hint.Base
#if __GLASGOW_HASKELL__ >= 611
import Hint.Annotations
#endif
import Hint.InterpreterT
import Hint.Configuration
import Hint.Context
import Hint.Reflection
import Hint.Typecheck
import Hint.Eval

import Control.Monad.Trans
