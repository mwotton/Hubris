module Hint.Conversions( FromGhcRep(..), FromGhcRep_(..), isSucceeded )

where

import qualified Hint.GHC as GHC

import Hint.Base
import qualified Hint.Compat as Compat

import Language.Haskell.Syntax ( HsModule(..), HsDecl(..), HsQualType )
import Language.Haskell.Parser ( parseModule, ParseResult(ParseOk) )

-- | Conversions from GHC representation to standard representations
class FromGhcRep ghc target where
    fromGhcRep :: MonadInterpreter m => ghc -> m target

class FromGhcRep_ ghc target where
    fromGhcRep_ :: ghc -> target

-- --------- Types / Kinds -----------------------

instance FromGhcRep GHC.Type HsQualType where
    fromGhcRep t =
        do t_str <- fromGhcRep t
           --
           let mod_str = unlines ["f ::" ++ t_str,
                                  "f = undefined"]
           let HsModule  _ _ _ _ [decl,_] = parseModule' mod_str
               HsTypeSig _ _ qualType     = decl
           --
           return qualType


instance FromGhcRep GHC.Type String where
    fromGhcRep t = do -- Unqualify necessary types
                      -- (i.e., do not expose internals)
                      unqual <- runGhc GHC.getPrintUnqual
                      return $ GHC.showSDocForUser unqual (Compat.pprType t)

parseModule' :: String -> HsModule
parseModule' s = case parseModule s of
                    ParseOk m -> m
                    failed    -> error $ unlines ["parseModulde' failed?!",
                                                  s,
                                                  show failed]

instance FromGhcRep_ Compat.Kind String where
    fromGhcRep_ (Compat.Kind k) = GHC.showSDoc (Compat.pprKind k)


-- ---------------- Modules --------------------------

instance FromGhcRep_ GHC.Module String where
    fromGhcRep_ = GHC.moduleNameString . GHC.moduleName

-- ---------------- Misc -----------------------------

isSucceeded :: GHC.SuccessFlag -> Bool
isSucceeded GHC.Succeeded = True
isSucceeded GHC.Failed    = False
