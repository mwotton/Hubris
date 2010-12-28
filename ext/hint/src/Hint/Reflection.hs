module Hint.Reflection (

      ModuleElem(..), Id, name, children,
      getModuleExports,

)

where

import Data.List
import Data.Maybe

import Hint.Base
import qualified Hint.GHC as GHC

-- | An Id for a class, a type constructor, a data constructor, a binding, etc
type Id = String

data ModuleElem = Fun Id | Class Id [Id] | Data Id [Id]
  deriving (Read, Show, Eq)

name :: ModuleElem -> Id
name (Fun f)     = f
name (Class c _) = c
name (Data d _)  = d

children :: ModuleElem -> [Id]
children (Fun   _)     = []
children (Class _ ms)  = ms
children (Data  _ dcs) = dcs


-- | Gets an abstract representation of all the entities exported by the module.
--   It is similar to the @:browse@ command in GHCi.
getModuleExports :: MonadInterpreter m => ModuleName -> m [ModuleElem]
getModuleExports mn =
    do module_  <- findModule mn
       mod_info <- mayFail $ runGhc1 GHC.getModuleInfo module_
       exports  <- mapM (runGhc1 GHC.lookupName) (GHC.modInfoExports mod_info)
       --
       return (asModElemList $ catMaybes exports)

asModElemList :: [GHC.TyThing] -> [ModuleElem]
asModElemList xs = concat [cs',
                           ts',
                           ds \\ (concatMap (map Fun . children) ts'),
                           fs \\ (concatMap (map Fun . children) cs')]
    where (cs,ts,ds,fs) = ([asModElem c | c@GHC.AClass{}   <- xs],
                           [asModElem t | t@GHC.ATyCon{}   <- xs],
                           [asModElem d | d@GHC.ADataCon{} <- xs],
                           [asModElem f | f@GHC.AnId{}     <- xs])
          cs' = [Class n $ filter (alsoIn fs) ms  | Class n ms  <- cs]
          ts' = [Data  t $ filter (alsoIn ds) dcs | Data  t dcs <- ts]
          alsoIn es = (`elem` (map name es))


asModElem :: GHC.TyThing -> ModuleElem
asModElem (GHC.AnId f)      = Fun $ getUnqualName f
asModElem (GHC.ADataCon dc) = Fun $ getUnqualName dc
asModElem (GHC.ATyCon tc)   = Data  (getUnqualName tc)
                                    (map getUnqualName $ GHC.tyConDataCons tc)
asModElem (GHC.AClass c)    = Class (getUnqualName c)
                                    (map getUnqualName $ GHC.classMethods c)

getUnqualName :: GHC.NamedThing a => a -> String
getUnqualName = GHC.showSDocUnqual . GHC.pprParenSymName
