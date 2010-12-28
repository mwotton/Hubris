-- - extract annotations from modules etc. with the GHC API.
-- - requires GHC >= 6.11
--
-- austin seipp <as@nijoruj.org>
module Hint.Annotations
( getModuleAnnotations -- :: (Data a, MonadInterpreter m) => a -> String -> m [a]
, getValAnnotations    -- :: (Data a, MonadInterpreter m) => a -> String -> m [a]
) where
import Control.Monad
import Data.Data
import Annotations
import Serialized

import Hint.Base
import HscTypes (hsc_mod_graph, ms_mod)
import qualified Hint.GHC as GHC

-- | Get the annotations associated with a particular module.
--
--   For example, given:
--
--   @
--   RBRACE-\# ANN module (1 :: Int) \#-LBRACE
--   module SomeModule(g, h) where
--   ...
--   @
--
--   Then after using 'loadModule' to load SomeModule into scope:
--
--   @
--   x <- getModuleAnnotations (as :: Int) "SomeModule"
--   liftIO $ print x
--   -- result is [1]
--   @
getModuleAnnotations :: (Data a, MonadInterpreter m) => a -> String -> m [a]
getModuleAnnotations _ x = do
  mods <- liftM hsc_mod_graph $ runGhc GHC.getSession
  let x' = filter ((==) x . GHC.moduleNameString . GHC.moduleName . ms_mod) mods
  v <- mapM (anns . ModuleTarget . ms_mod) x'
  return $ concat v

-- | Get the annotations associated with a particular function
--
--   For example, given:
--
--   @
--   module SomeModule(g, h) where
--
--   LBRACE-\# ANN g (Just 1 :: Maybe Int) \#-RBRACE
--   g = f [f]
--
--   LBRACE-\# ANN h (Just 2 :: Maybe Int) \#-RBRACE
--   h = f
--   @
--
--   Then after using 'loadModule' to bring SomeModule into scope:
--
--   @
--   x <- liftM concat $ mapM (getValAnnotations (as :: Maybe Int)) [\"g\",\"h\"]
--   liftIO $ print x
--   -- result is [Just 2, Just 1]
--   @
--
--   This can also work on data constructors and types with annotations.
getValAnnotations :: (Data a, MonadInterpreter m) => a -> String -> m [a]
getValAnnotations _ x = do
  x' <- runGhc1 GHC.parseName x
  v <- mapM (anns . NamedTarget) x'
  return $ concat v

anns :: (MonadInterpreter m, Data a) => AnnTarget GHC.Name -> m [a]
anns = runGhc1 (GHC.findGlobalAnns deserializeWithData)
