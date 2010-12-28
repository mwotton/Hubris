module Hint.SignalHandlers (
    protectHandlers
)
where

import Control.Monad.CatchIO
import Control.Monad.Trans

#ifdef mingw32_HOST_OS
import GHC.ConsoleHandler as C

saveHandlers :: MonadCatchIO m => m C.Handler
saveHandlers = liftIO $ C.installHandler Ignore

restoreHandlers :: MonadCatchIO m => C.Handler -> m C.Handler
restoreHandlers = liftIO . C.installHandler

#else
import qualified System.Posix.Signals as S

helper :: MonadCatchIO m => S.Handler -> S.Signal -> m S.Handler
helper handler signal = liftIO $ S.installHandler signal handler Nothing

signals :: [S.Signal]
signals = [ S.sigQUIT
          , S.sigINT
          , S.sigHUP
          , S.sigTERM
          ]

saveHandlers :: MonadCatchIO m => m [S.Handler]
saveHandlers = liftIO $ mapM (helper S.Ignore) signals

restoreHandlers :: MonadCatchIO m => [S.Handler] -> m [S.Handler]
restoreHandlers h  = liftIO . sequence $ zipWith helper h signals

#endif

protectHandlers :: MonadCatchIO m => m a -> m a
protectHandlers a = bracket saveHandlers restoreHandlers $ const a
