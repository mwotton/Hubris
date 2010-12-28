module Hint.Base (
    MonadInterpreter(..), RunGhc,
    --
    GhcError(..), InterpreterError(..), mayFail,
    --
    InterpreterSession, SessionData(..), GhcErrLogger,
    InterpreterState(..), fromState, onState,
    InterpreterConfiguration(..),
    --
    runGhc1, runGhc2, runGhc3, runGhc4, runGhc5,
    --
    ModuleName, PhantomModule(..),
    findModule, moduleIsLoaded,
    --
    ghcVersion
)

where

import Control.Monad.Error
import Control.Monad.CatchIO

import Data.IORef
import Data.Dynamic

import qualified Hint.GHC as GHC

import Hint.Extension

-- | Version of the underlying ghc api. Values are:
--
-- * @606@ for GHC 6.6.x
--
-- * @608@ for GHC 6.8.x
--
-- * @610@ for GHC 6.10.x
--
-- * etc...
ghcVersion :: Int
ghcVersion = __GLASGOW_HASKELL__

-- this requires FlexibleContexts
class (MonadCatchIO m,MonadError InterpreterError m) => MonadInterpreter m where
    fromSession      :: FromSession m a
    modifySessionRef :: ModifySessionRef m a
    runGhc           :: RunGhc m a

-- this is for hiding the actual types in haddock
type FromSession      m a = (InterpreterSession -> a) -> m a
type ModifySessionRef m a = (InterpreterSession -> IORef a) -> (a -> a) -> m a


data InterpreterError = UnknownError String
                      | WontCompile [GhcError]
                      | NotAllowed  String
                      -- | GhcExceptions from the underlying GHC API are caught
                      -- and rethrown as this.
                      | GhcException String
                      deriving (Show, Typeable)

instance Error InterpreterError where
    noMsg  = UnknownError ""
    strMsg = UnknownError

data InterpreterState = St{active_phantoms      :: [PhantomModule],
                           zombie_phantoms      :: [PhantomModule],
                           hint_support_module  :: PhantomModule,
                           import_qual_hack_mod :: Maybe PhantomModule,
                           qual_imports         :: [(ModuleName, String)],
                           configuration        :: InterpreterConfiguration}

data InterpreterConfiguration = Conf {
                                  search_path       :: [FilePath],
                                  language_exts     :: [Extension],
                                  all_mods_in_scope :: Bool
                                }

#if __GLASGOW_HASKELL__ < 610
type InterpreterSession = SessionData GHC.Session

adjust :: (a -> b -> c) -> (b -> a -> c)
adjust f = flip f

type RunGhc  m a           = (GHC.Session -> IO a)
                          -> m a
type RunGhc1 m a b         = (GHC.Session -> a -> IO b)
                          -> (a -> m b)
type RunGhc2 m a b c       = (GHC.Session -> a -> b -> IO c)
                          -> (a -> b -> m c)
type RunGhc3 m a b c d     = (GHC.Session -> a -> b -> c -> IO d)
                          -> (a -> b -> c -> m d)

type RunGhc4 m a b c d e   = (GHC.Session -> a -> b -> c -> d -> IO e)
                          -> (a -> b -> c -> d -> m e)

type RunGhc5 m a b c d e f = (GHC.Session -> a -> b -> c -> d -> e -> IO f)
                          -> (a -> b -> c -> d -> e -> m f)

#else
      -- ghc >= 6.10
type InterpreterSession = SessionData ()

instance Exception InterpreterError

adjust :: (a -> b) -> (a -> b)
adjust = id

type RunGhc  m a =
    (forall n.(MonadCatchIO n,Functor n) => GHC.GhcT n a)
 -> m a

type RunGhc1 m a b =
    (forall n.(MonadCatchIO n, Functor n) => a -> GHC.GhcT n b)
 -> (a -> m b)

type RunGhc2 m a b c =
    (forall n.(MonadCatchIO n, Functor n) => a -> b -> GHC.GhcT n c)
 -> (a -> b -> m c)

type RunGhc3 m a b c d =
    (forall n.(MonadCatchIO n, Functor n) => a -> b -> c -> GHC.GhcT n d)
 -> (a -> b -> c -> m d)

type RunGhc4 m a b c d e =
    (forall n.(MonadCatchIO n, Functor n) => a -> b -> c -> d -> GHC.GhcT n e)
 -> (a -> b -> c -> d -> m e)

type RunGhc5 m a b c d e f =
    (forall n.(MonadCatchIO n, Functor n) => a->b->c->d->e->GHC.GhcT n f)
 -> (a -> b -> c -> d -> e -> m f)
#endif

data SessionData a = SessionData {
                       internalState   :: IORef InterpreterState,
                       versionSpecific :: a,
                       ghcErrListRef   :: IORef [GhcError],
                       ghcErrLogger    :: GhcErrLogger
                     }

-- When intercepting errors reported by GHC, we only get a ErrUtils.Message
-- and a SrcLoc.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
newtype GhcError = GhcError{errMsg :: String} deriving Show

mapGhcExceptions :: MonadInterpreter m
                 => (String -> InterpreterError)
                 -> m a
                 -> m a
mapGhcExceptions buildEx action =
    do  action
          `catchError` (\err -> case err of
                                    GhcException s -> throwError (buildEx s)
                                    _              -> throwError err)

type GhcErrLogger = GHC.Severity
                 -> GHC.SrcSpan
                 -> GHC.PprStyle
                 -> GHC.Message
                 -> IO ()

-- | Module names are _not_ filepaths.
type ModuleName = String

runGhc1 :: MonadInterpreter m => RunGhc1 m a b
runGhc1 f a = runGhc (adjust f a)

runGhc2 :: MonadInterpreter m => RunGhc2 m a b c
runGhc2 f a = runGhc1 (adjust f a)

runGhc3 :: MonadInterpreter m => RunGhc3 m a b c d
runGhc3 f a = runGhc2 (adjust f a)

runGhc4 :: MonadInterpreter m => RunGhc4 m a b c d e
runGhc4 f a = runGhc3 (adjust f a)

runGhc5 :: MonadInterpreter m => RunGhc5 m a b c d e f
runGhc5 f a = runGhc4 (adjust f a)


-- ================ Handling the interpreter state =================

fromState :: MonadInterpreter m => (InterpreterState -> a) -> m a
fromState f = do ref_st <- fromSession internalState
                 liftIO $ f `fmap` readIORef ref_st

onState :: MonadInterpreter m => (InterpreterState -> InterpreterState) -> m ()
onState f = modifySessionRef internalState f >> return ()

-- =============== Error handling ==============================

mayFail :: MonadInterpreter m => m (Maybe a) -> m a
mayFail action =
    do
        maybe_res <- action
        --
        es <- modifySessionRef ghcErrListRef (const [])
        --
        case (maybe_res, null es) of
            (Nothing,True)  -> throwError $ UnknownError "Got no error message"
            (Nothing,False) -> throwError $ WontCompile (reverse es)
            (Just a, True)  -> return a
            (Just _, False) -> fail $ "GHC returned a result but said: " ++
                                      show es

-- ================ Misc ===================================

-- this type ought to go in Hint.Context, but ghc dislikes cyclic imports...
data PhantomModule = PhantomModule{pm_name :: ModuleName, pm_file :: FilePath}
                   deriving (Eq, Show)

findModule :: MonadInterpreter m => ModuleName -> m GHC.Module
findModule mn = mapGhcExceptions NotAllowed $
                    runGhc2 GHC.findModule mod_name Nothing
    where mod_name = GHC.mkModuleName mn


moduleIsLoaded :: MonadInterpreter m => ModuleName -> m Bool
moduleIsLoaded mn = (findModule mn >> return True)
                   `catchError` (\e -> case e of
                                        NotAllowed{} -> return False
                                        _            -> throwError e)
