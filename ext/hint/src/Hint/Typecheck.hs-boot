module Hint.Typecheck where

import Hint.Base (MonadInterpreter)

typeChecks_unsandboxed :: MonadInterpreter m => String -> m Bool
