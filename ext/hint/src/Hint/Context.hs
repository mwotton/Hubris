module Hint.Context (

      ModuleName, isModuleInterpreted,
      loadModules, getLoadedModules, setTopLevelModules,
      setImports, setImportsQ,
      reset,

      PhantomModule(..), ModuleText,
      addPhantomModule, removePhantomModule, getPhantomModules,

      allModulesInContext, onAnEmptyContext,

      support_String, support_show
)

where

import Prelude hiding ( mod )

import Data.Char
import Data.List
import Data.Maybe

import Control.Monad       ( liftM, filterM, when, guard )
import Control.Monad.Error ( catchError, throwError, liftIO )

import Hint.Base
import Hint.Util ( (>=>) ) -- compat version
import Hint.Conversions
import qualified Hint.Util   as Util
import qualified Hint.Compat as Compat

import qualified Hint.GHC as GHC

import System.Random
import System.FilePath
import System.Directory
import qualified System.IO.UTF8 as UTF8 (writeFile)

type ModuleText = String

-- When creating a phantom module we have a situation similar to that of
-- @Hint.Util.safeBndFor@: we want to avoid picking a module name that is
-- already in-scope. Additionally, since this may be used with sandboxing in
-- mind we want to avoid easy-to-guess names. Thus, we do a trick similar
-- to the one in safeBndFor, but including a random number instead of an
-- additional digit
newPhantomModule :: MonadInterpreter m => m PhantomModule
newPhantomModule =
    do n <- liftIO randomIO
       (ls,is) <- allModulesInContext
       let nums = concat [show (abs n::Int), filter isDigit $ concat (ls ++ is)]
       let mod_name = 'M':nums
       --
       tmp_dir <- liftIO getTemporaryDirectory
       --
       return PhantomModule{pm_name = mod_name, pm_file = tmp_dir </> nums}

allModulesInContext :: MonadInterpreter m => m ([ModuleName], [ModuleName])
allModulesInContext =
    do (l, i) <- runGhc Compat.getContext
       return (map fromGhcRep_ l, map fromGhcRep_ i)

addPhantomModule :: MonadInterpreter m
                 => (ModuleName -> ModuleText)
                 -> m PhantomModule
addPhantomModule mod_text =
    do pm <- newPhantomModule
       let t  = Compat.fileTarget (pm_file pm)
           m  = GHC.mkModuleName (pm_name pm)
       --
       liftIO $ UTF8.writeFile (pm_file pm) (mod_text $ pm_name pm)
       --
       onState (\s -> s{active_phantoms = pm:active_phantoms s})
       mayFail (do -- GHC.load will remove all the modules from scope, so first
                   -- we save the context...
                   (old_top, old_imps) <- runGhc Compat.getContext
                   --
                   runGhc1 GHC.addTarget t
                   res <- runGhc1 GHC.load (GHC.LoadUpTo m)
                   --
                   if isSucceeded res
                     then do runGhc2 Compat.setContext old_top old_imps
                             return $ Just ()
                     else return Nothing)
        `catchError` (\err -> case err of
                                WontCompile _ -> do removePhantomModule pm
                                                    throwError err
                                _             -> throwError err)
       --
       return pm

removePhantomModule :: MonadInterpreter m => PhantomModule -> m ()
removePhantomModule pm =
    do -- We don't want to actually unload this module, because that
       -- would mean that all the real modules might get reloaded and the
       -- user didn't require that (they may be in a non-compiling state!).
       -- However, this means that we can't actually delete the file, because
       -- it is an active target. Therefore, we simply take it out of scope
       -- and mark it as "delete me when possible" (i.e., next time the
       -- @loadModules@ function is called).
       --
       isLoaded <- moduleIsLoaded $ pm_name pm
       safeToRemove <-
           if isLoaded
             then do -- take it out of scope
                     mod <- findModule (pm_name pm)
                     (mods, imps) <- runGhc Compat.getContext
                     let mods' = filter (mod /=) mods
                     runGhc2 Compat.setContext mods' imps
                     --
                     let isNotPhantom = isPhantomModule . fromGhcRep_  >=>
                                          return . not
                     null `liftM` filterM isNotPhantom mods'
             else return True
       --
       let file_name = pm_file pm
       runGhc1 GHC.removeTarget (Compat.targetId $ Compat.fileTarget file_name)
       --
       onState (\s -> s{active_phantoms = filter (pm /=) $ active_phantoms s})
       --
       if safeToRemove
         then do mayFail $ do res <- runGhc1 GHC.load GHC.LoadAllTargets
                              return $ guard (isSucceeded res) >> Just ()
                 liftIO $ removeFile (pm_file pm)
         else do onState (\s -> s{zombie_phantoms = pm:zombie_phantoms s})
                 return ()

-- Returns a tuple with the active and zombie phantom modules respectively
getPhantomModules :: MonadInterpreter m => m ([PhantomModule], [PhantomModule])
getPhantomModules = do active <- fromState active_phantoms
                       zombie <- fromState zombie_phantoms
                       return (active, zombie)

isPhantomModule :: MonadInterpreter m => ModuleName -> m Bool
isPhantomModule mn = do (as,zs) <- getPhantomModules
                        return $ mn `elem` (map pm_name $ as ++ zs)

-- | Tries to load all the requested modules from their source file.
--   Modules my be indicated by their ModuleName (e.g. \"My.Module\") or
--   by the full path to its source file.
--
-- The interpreter is 'reset' both before loading the modules and in the event
-- of an error.
loadModules :: MonadInterpreter m => [String] -> m ()
loadModules fs = do -- first, unload everything, and do some clean-up
                    reset
                    doLoad fs `catchError` (\e -> reset >> throwError e)

doLoad :: MonadInterpreter m => [String] -> m ()
doLoad fs = do mayFail $ do
                   targets <- mapM (\f->runGhc2 Compat.guessTarget f Nothing) fs
                   --
                   runGhc1 GHC.setTargets targets
                   res <- runGhc1 GHC.load GHC.LoadAllTargets
                   -- loading the targets removes the support module
                   reinstallSupportModule
                   return $ guard (isSucceeded res) >> Just ()

-- | Returns True if the module was interpreted.
isModuleInterpreted :: MonadInterpreter m => ModuleName -> m Bool
isModuleInterpreted m = findModule m >>= runGhc1 GHC.moduleIsInterpreted

-- | Returns the list of modules loaded with 'loadModules'.
getLoadedModules :: MonadInterpreter m => m [ModuleName]
getLoadedModules = do (active_pms, zombie_pms) <- getPhantomModules
                      ms <- map modNameFromSummary `liftM` getLoadedModSummaries
                      return $ ms \\ (map pm_name $ active_pms ++ zombie_pms)

modNameFromSummary :: GHC.ModSummary -> ModuleName
modNameFromSummary =  fromGhcRep_ . GHC.ms_mod

getLoadedModSummaries :: MonadInterpreter m => m [GHC.ModSummary]
getLoadedModSummaries =
  do all_mod_summ <- runGhc GHC.getModuleGraph
     filterM (runGhc1 GHC.isLoaded . GHC.ms_mod_name) all_mod_summ

-- | Sets the modules whose context is used during evaluation. All bindings
--   of these modules are in scope, not only those exported.
--
--   Modules must be interpreted to use this function.
setTopLevelModules :: MonadInterpreter m => [ModuleName] -> m ()
setTopLevelModules ms =
    do loaded_mods_ghc <- getLoadedModSummaries
       --
       let not_loaded = ms \\ map modNameFromSummary loaded_mods_ghc
       when (not . null $ not_loaded) $
         throwError $ NotAllowed ("These modules have not been loaded:\n" ++
                                  unlines not_loaded)
       --
       active_pms <- fromState active_phantoms
       ms_mods <- mapM findModule (nub $ ms ++ map pm_name active_pms)
       --
       let mod_is_interpr = runGhc1 GHC.moduleIsInterpreted
       not_interpreted <- filterM (liftM not . mod_is_interpr) ms_mods
       when (not . null $ not_interpreted) $
         throwError $ NotAllowed ("These modules are not interpreted:\n" ++
                                  unlines (map fromGhcRep_ not_interpreted))
       --
       (_, old_imports) <- runGhc Compat.getContext
       runGhc2 Compat.setContext ms_mods old_imports

onAnEmptyContext :: MonadInterpreter m => m a -> m a
onAnEmptyContext action =
    do (old_mods, old_imps) <- runGhc Compat.getContext
       runGhc2 Compat.setContext [] []
       let restore = runGhc2 Compat.setContext old_mods old_imps
       a <- action `catchError` (\e -> do restore; throwError e)
       restore
       return a

-- | Sets the modules whose exports must be in context.
--
--   Warning: 'setImports' and 'setImportsQ' are mutually exclusive.
--   If you have a list of modules to be used qualified and another list
--   unqualified, then you need to do something like
--
--   >  setImportsQ ((zip unqualified $ repeat Nothing) ++ qualifieds)
setImports :: MonadInterpreter m => [ModuleName] -> m ()
setImports ms = setImportsQ $ zip ms (repeat Nothing)

-- | Sets the modules whose exports must be in context; some
--   of them may be qualified. E.g.:
--
--   @setImports [("Prelude", Nothing), ("Data.Map", Just "M")]@.
--
--   Here, "map" will refer to Prelude.map and "M.map" to Data.Map.map.
setImportsQ :: MonadInterpreter m => [(ModuleName, Maybe String)] -> m ()
setImportsQ ms =
    do let (q,     u) = Util.partition (isJust . snd) ms
           (quals, unquals) = (map (\(a, Just b) -> (a,b)) q, map fst u)
       --
       unqual_mods <- mapM findModule unquals
       mapM_ (findModule . fst) quals -- just to be sure they exist
       --
       old_qual_hack_mod <- fromState import_qual_hack_mod
       maybe (return ()) removePhantomModule old_qual_hack_mod
       --
       new_pm <- if ( not $ null quals )
                   then do
                     new_pm <- addPhantomModule $ \mod_name -> unlines $
                                ("module " ++ mod_name ++ " where ") :
                                ["import qualified " ++ m ++ " as " ++ n |
                                   (m,n) <- quals]
                     onState (\s -> s{import_qual_hack_mod = Just new_pm})
                     return $ Just new_pm
                   else return Nothing
       --
       pm <- maybe (return []) (findModule . pm_name >=> return . return) new_pm
       (old_top_level, _) <- runGhc Compat.getContext
       let new_top_level = pm ++ old_top_level
       runGhc2 Compat.setContext new_top_level unqual_mods
       --
       onState (\s ->s{qual_imports = quals})

-- | All imported modules are cleared from the context, and
--   loaded modules are unloaded. It is similar to a @:load@ in
--   GHCi, but observe that not even the Prelude will be in
--   context after a reset.
reset :: MonadInterpreter m => m ()
reset =
    do -- Remove all modules from context
       runGhc2 Compat.setContext [] []
       --
       -- Unload all previously loaded modules
       runGhc1 GHC.setTargets []
       _ <- runGhc1 GHC.load GHC.LoadAllTargets
       --
       -- At this point, GHCi would call rts_revertCAFs and
       -- reset the buffering of stdin, stdout and stderr.
       -- Should we do any of these?
       --
       -- liftIO $ rts_revertCAFs
       --
       -- We now remove every phantom module and forget about qual imports
       old_active <- fromState active_phantoms
       old_zombie <- fromState zombie_phantoms
       onState (\s -> s{active_phantoms      = [],
                        zombie_phantoms      = [],
                        import_qual_hack_mod = Nothing,
                        qual_imports         = []})
       liftIO $ mapM_ (removeFile . pm_file) (old_active ++ old_zombie)
       --
       -- Now, install a support module
       installSupportModule

-- Load a phantom module with all the symbols from the prelude we need
installSupportModule :: MonadInterpreter m => m ()
installSupportModule = do mod <- addPhantomModule support_module
                          onState (\st -> st{hint_support_module = mod})
                          mod' <- findModule (pm_name mod)
                          runGhc2 Compat.setContext [mod'] []
    --
    where support_module m = unlines [
                               "module " ++ m ++ "( ",
                               "    " ++ _String ++ ",",
                               "    " ++ _show   ++ ")",
                               "where",
                               "",
                               "import qualified Prelude as P",
                               "",
                               "type " ++ _String ++ " = P.String",
                               "",
                               _show ++ " :: P.Show a => a -> P.String",
                               _show ++ " = P.show"
                             ]
            where _String = altStringName m
                  _show   = altShowName m

-- Call it when the support module is an active phantom module but has been
-- unloaded as a side effect by GHC (e.g. by calling GHC.loadTargets)
reinstallSupportModule :: MonadInterpreter m => m ()
reinstallSupportModule = do pm <- fromState hint_support_module
                            removePhantomModule pm
                            installSupportModule

altStringName :: ModuleName -> String
altStringName mod_name = "String_" ++ mod_name

altShowName :: ModuleName -> String
altShowName mod_name = "show_" ++ mod_name

support_String :: MonadInterpreter m => m String
support_String = do mod_name <- fromState (pm_name . hint_support_module)
                    return $ concat [mod_name, ".", altStringName mod_name]

support_show :: MonadInterpreter m => m String
support_show = do mod_name <- fromState (pm_name . hint_support_module)
                  return $ concat [mod_name, ".", altShowName mod_name]

-- SHOULD WE CALL THIS WHEN MODULES ARE LOADED / UNLOADED?
-- foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
