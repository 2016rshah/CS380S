module InstrumentationTrav(
InstTrav,
log, logPretty,
addPreservingFn, addNonPreservingFn,
getPreservingFns, getNonPreservingFns,
addTransformedFn,
addToTaints, getTaintValue, getTaintMap, setTaintMap
)
where
import Prelude hiding (log)

import EntropicDependency
import InstrumentationState

import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Pretty

import qualified Data.Map as Map

type InstTrav = Trav InstrumentationState 

log :: (Show a) => a -> InstTrav ()
log = modifyUserState . addToLog

logPretty :: (Pretty a) => a -> InstTrav ()
logPretty = modifyUserState . addPrettyToLog

getPreservingFns :: InstTrav [Decl]
getPreservingFns = preservingFns <$> getUserState

getNonPreservingFns :: InstTrav [Decl]
getNonPreservingFns = nonPreservingFns <$> getUserState

addPreservingFn :: Decl -> InstTrav ()
addPreservingFn f = modifyUserState $ \is -> is { preservingFns = preservingFns is ++ [f] }

addNonPreservingFn :: Decl -> InstTrav ()
addNonPreservingFn f = modifyUserState $ \is -> is { nonPreservingFns = nonPreservingFns is ++ [f] }

addTransformedFn :: (FunDef, FunDef) -> InstTrav ()
addTransformedFn f = modifyUserState $ \is -> is { transformedFns = transformedFns is ++ [f] }

addToTaints :: Ident -> EntropicDependency -> InstTrav ()
addToTaints id t = modifyUserState $ \is -> is { taints = Map.insert id t (taints is) }

getTaintValue :: Ident -> InstTrav EntropicDependency
getTaintValue id = do
    st <- getUserState
    return $ Map.findWithDefault NoDependency id $ taints st

getTaintMap :: InstTrav Taints
getTaintMap = taints <$> getUserState

setTaintMap :: Taints -> InstTrav ()
setTaintMap tm = modifyUserState (\st -> st { taints = tm })