module Hint.InterpreterT (
    InterpreterT, Interpreter, runInterpreter, runInterpreterWithArgs,
    MultipleInstancesNotAllowed(..)
)

where

import Prelude hiding ( catch )

import Hint.Base
import Hint.Context
import Hint.Configuration

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.CatchIO

import Data.Typeable ( Typeable )
import Control.Concurrent.MVar
import System.IO.Unsafe ( unsafePerformIO )

import Data.IORef
import Data.List
#if __GLASGOW_HASKELL__ < 610
import Data.Dynamic
#endif

import qualified GHC.Paths

import qualified Hint.GHC as GHC
import qualified Hint.Compat as Compat

type Interpreter = InterpreterT IO

#if __GLASGOW_HASKELL__ < 610

newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT InterpreterSession
                                               (ErrorT InterpreterError m) a}
    deriving (Functor, Monad, MonadIO, MonadCatchIO)

execute :: (MonadCatchIO m, Functor m)
        => InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute s = runErrorT . flip runReaderT s . unInterpreterT

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

runGhc_impl :: (MonadCatchIO m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl f = do s <- fromSession versionSpecific -- i.e. the ghc session
                   r <- liftIO $ f' s
                   either throwError return r
    where f' = tryJust (fmap (GhcException . showGhcEx) . ghcExceptions) . f
          ghcExceptions (DynException e) = fromDynamic e
          ghcExceptions  _               = Nothing

#else
      -- ghc >= 6.10
newtype InterpreterT m a = InterpreterT{
                             unInterpreterT :: ReaderT  InterpreterSession
                                              (ErrorT   InterpreterError
                                              (GHC.GhcT m)) a}
    deriving (Functor, Monad, MonadIO, MonadCatchIO)

execute :: (MonadCatchIO m, Functor m)
        => InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute s = GHC.runGhcT (Just GHC.Paths.libdir)
          . runErrorT
          . flip runReaderT s
          . unInterpreterT

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift . lift

runGhc_impl :: (MonadCatchIO m, Functor m) => RunGhc (InterpreterT m) a
runGhc_impl a = InterpreterT (lift (lift a))
                `catches`
                 [Handler (\(e :: GHC.SourceError)  -> rethrowWC e),
                  Handler (\(e :: GHC.GhcApiError)  -> rethrowGE $ show e),
                  Handler (\(e :: GHC.GhcException) -> rethrowGE $ showGhcEx e)]
    where rethrowGE = throwError . GhcException
          rethrowWC = throwError
                    . WontCompile
                    . map (GhcError . show)
                    . GHC.bagToList
                    . GHC.srcErrorMessages
#endif

showGhcEx :: GHC.GhcException -> String
showGhcEx = flip GHC.showGhcException ""

-- ================= Executing the interpreter ==================

initialize :: (MonadCatchIO m, Functor m)
              => [String]
              -> InterpreterT m ()
initialize args =
    do log_handler <- fromSession ghcErrLogger
       -- Set a custom log handler, to intercept error messages :S
       dflags <- runGhc GHC.getSessionDynFlags

       dflags' <- do
           let df = Compat.configureDynFlags dflags

           (df', extra) <- runGhc2 Compat.parseDynamicFlags df args
           when (not . null $ extra) $
               throwError $ UnknownError (concat [ "flags: '"
                                                 , intercalate " " extra
                                                 , "' not recognized"])
           return df'

       -- Observe that, setSessionDynFlags loads info on packages
       -- available; calling this function once is mandatory!
       _ <- runGhc1 GHC.setSessionDynFlags dflags'{GHC.log_action = log_handler}

       reset

-- | Executes the interpreter. Returns @Left InterpreterError@ in case of error.
--
-- NB. The underlying ghc will overwrite certain signal handlers
-- (SIGINT, SIGHUP, SIGTERM, SIGQUIT on Posix systems, Ctrl-C handler on Windows).
-- In future versions of hint, this might be controlled by the user.
runInterpreter :: (MonadCatchIO m, Functor m)
               => InterpreterT m a
               -> m (Either InterpreterError a)
runInterpreter = runInterpreterWithArgs []

-- | Executes the interpreter, setting args passed in as though they
-- were command-line args. Returns @Left InterpreterError@ in case of
-- error.
runInterpreterWithArgs :: (MonadCatchIO m, Functor m)
                          => [String]
                          -> InterpreterT m a
                          -> m (Either InterpreterError a)
runInterpreterWithArgs args action =
  ifInterpreterNotRunning $
    do s <- newInterpreterSession `catch` rethrowGhcException
       -- SH.protectHandlers $ execute s (initialize args >> action)
       execute s (initialize args >> action)
    where rethrowGhcException   = throw . GhcException . showGhcEx
#if __GLASGOW_HASKELL__ < 610
          newInterpreterSession =  do s <- liftIO $
                                             Compat.newSession GHC.Paths.libdir
                                      newSessionData s
#else
          -- GHC >= 610
          newInterpreterSession = newSessionData ()
#endif

{-# NOINLINE uniqueToken #-}
uniqueToken :: MVar ()
uniqueToken = unsafePerformIO $ newMVar ()

ifInterpreterNotRunning :: MonadCatchIO m => m a -> m a
ifInterpreterNotRunning action =
    do maybe_token <- liftIO $ tryTakeMVar uniqueToken
       case maybe_token of
           Nothing -> throw MultipleInstancesNotAllowed
           Just x  -> action `finally` (liftIO $ putMVar uniqueToken x)

-- | The installed version of ghc is not thread-safe. This exception
--   is thrown whenever you try to execute @runInterpreter@ while another
--   instance is already running.
data MultipleInstancesNotAllowed = MultipleInstancesNotAllowed deriving Typeable

instance Exception MultipleInstancesNotAllowed

instance Show MultipleInstancesNotAllowed where
    show _ = "This version of GHC is not thread-safe," ++
             "can't safely run two instances of the interpreter simultaneously"

initialState :: InterpreterState
initialState = St {active_phantoms      = [],
                   zombie_phantoms      = [],
                   hint_support_module  = error "No support module loaded!",
                   import_qual_hack_mod = Nothing,
                   qual_imports         = [],
                   configuration        = defaultConf}


newSessionData :: MonadIO m => a -> m (SessionData a)
newSessionData  a = do initial_state    <- liftIO $ newIORef initialState
                       ghc_err_list_ref <- liftIO $ newIORef []
                       return SessionData{
                                internalState   = initial_state,
                                versionSpecific = a,
                                ghcErrListRef   = ghc_err_list_ref,
                                ghcErrLogger    = mkLogHandler ghc_err_list_ref
                              }

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r _ src style msg = modifyIORef r (errorEntry :)
    where errorEntry = mkGhcError src style msg

mkGhcError :: GHC.SrcSpan -> GHC.PprStyle -> GHC.Message -> GhcError
mkGhcError src_span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = GHC.showSDoc . GHC.withPprStyle style $
                         GHC.mkLocMessage src_span msg


-- The MonadInterpreter instance

instance (MonadCatchIO m, Functor m) => MonadInterpreter (InterpreterT m) where
    fromSession f = InterpreterT $ fmap f ask
    --
    modifySessionRef target f =
        do ref     <- fromSession target
           old_val <- liftIO $ atomicModifyIORef ref (\a -> (f a, a))
           return old_val
    --
    runGhc a = runGhc_impl a

instance Monad m => MonadError InterpreterError (InterpreterT m) where
    throwError  = InterpreterT . throwError
    catchError (InterpreterT m) catchE = InterpreterT $
                                             m `catchError`
                                              (\e -> unInterpreterT $ catchE e)

instance (Monad m, Applicative m) => Applicative (InterpreterT m) where
    pure  = return
    (<*>) = ap
