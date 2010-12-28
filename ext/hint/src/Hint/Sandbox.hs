module Hint.Sandbox ( sandboxed ) where

import Hint.Base
import Hint.Context
import Hint.Configuration
import Hint.Util

import {-# SOURCE #-} Hint.Typecheck ( typeChecks_unsandboxed )

import Control.Monad.Error

sandboxed :: MonadInterpreter m => (Expr -> m a) -> (Expr -> m a)
sandboxed = if ghcVersion >= 610 then id else old_sandboxed

old_sandboxed :: MonadInterpreter m => (Expr -> m a) -> (Expr -> m a)
old_sandboxed do_stuff = \expr -> do no_sandbox <- fromConf all_mods_in_scope
                                     if no_sandbox
                                       then do_stuff expr
                                       else usingAModule do_stuff expr

usingAModule :: MonadInterpreter m => (Expr -> m a) -> (Expr -> m a)
usingAModule do_stuff_on = \expr ->
       --
       -- To avoid defaulting, we will evaluate this expression without the
       -- monomorphism-restriction. This means that expressions that normally
       -- would not typecheck, suddenly will. Thus, we first check if the
       -- expression typechecks as is. If it doesn't, there is no need in
       -- going on (if it does, it may not typecheck once we restrict the
       -- context; that is the whole idea of this!)
       --
    do type_checks <- typeChecks_unsandboxed expr
       case type_checks of
         False -> do_stuff_on expr -- fail as you wish...
         True  ->
             do (loaded, imports) <- allModulesInContext
                zombies           <- fromState zombie_phantoms
                quals             <- fromState qual_imports
                --
                let e = safeBndFor expr
                let mod_text no_prel mod_name = textify [
                     ["{-# LANGUAGE NoMonomorphismRestriction #-}"],
                     ["{-# LANGUAGE NoImplicitPrelude #-}" | no_prel],
                     ["module " ++ mod_name],
                     ["where"],
                     ["import " ++ m | m <- loaded ++ imports,
                                       not $ m `elem` (map pm_name zombies)],
                     ["import qualified " ++ m ++ " as " ++ q | (m,q) <- quals],
                     [e ++ " = " ++ expr] ]
                --
                let go no_prel = do pm <- addPhantomModule (mod_text no_prel)
                                    setTopLevelModules [pm_name pm]
                                    r <- do_stuff_on e
                                          `catchError` (\err ->
                                             case err of
                                               WontCompile _ ->
                                                      do removePhantomModule pm
                                                         throwError err
                                               _ -> throwError err)
                                    removePhantomModule pm
                                    return r
                -- If the Prelude was not explicitly imported but implicitly
                -- imported in some interpreted module, then the user may
                -- get very unintuitive errors when turning sandboxing on. Thus
                -- we will import the Prelude if the operation fails...
                -- I guess this may lead to even more obscure errors, but
                -- hopefully in much less frequent situations...
                r <- onAnEmptyContext $ go True
                      `catchError` (\err -> case err of
                                             WontCompile _ -> go False
                                             _             -> throwError err)
                --
                return r
           --
           where textify    = unlines . concat
