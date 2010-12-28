module Hint.Typecheck (

      typeOf, typeChecks, kindOf,

      typeOf_unsandboxed, typeChecks_unsandboxed

)

where

import Control.Monad.Error

import Hint.Base
import Hint.Parsers
import Hint.Conversions
import Hint.Sandbox

import qualified Hint.Compat as Compat

-- | Returns a string representation of the type of the expression.
typeOf :: MonadInterpreter m => String -> m String
typeOf = sandboxed typeOf_unsandboxed

typeOf_unsandboxed :: MonadInterpreter m => String -> m String
typeOf_unsandboxed expr =
    do -- First, make sure the expression has no syntax errors,
       -- for this is the only way we have to "intercept" this
       -- kind of errors
       failOnParseError parseExpr expr
       --
       ty <- mayFail $ runGhc1 Compat.exprType expr
       --
       fromGhcRep ty

-- | Tests if the expression type checks.
typeChecks :: MonadInterpreter m => String -> m Bool
typeChecks = sandboxed typeChecks_unsandboxed

typeChecks_unsandboxed :: MonadInterpreter m => String -> m Bool
typeChecks_unsandboxed expr = (typeOf_unsandboxed expr >> return True)
                              `catchError`
                              onCompilationError (\_ -> return False)

-- | Returns a string representation of the kind of the type expression.
kindOf :: MonadInterpreter m => String -> m String
kindOf = sandboxed go
    where go type_expr =
              do -- First, make sure the expression has no syntax errors,
                 -- for this is the only way we have to "intercept" this
                 -- kind of errors
                 failOnParseError parseType type_expr
                 --
                 kind <- mayFail $ runGhc1 Compat.typeKind type_expr
                 --
                 return $ fromGhcRep_ (Compat.Kind kind)

onCompilationError :: MonadInterpreter m
                   => ([GhcError] -> m a)
                   -> (InterpreterError -> m a)
onCompilationError recover =
    \interp_error -> case interp_error of
                       WontCompile errs -> recover errs
                       otherErr         -> throwError otherErr
