module InstrumentationTrav(
InstTrav,
log, logPretty,
addPreservingFn, addNonPreservingFn,
getPreservingFns, getNonPreservingFns,
addTransformedFn,
addToTaints, getTaintValue, getTaintMap, setTaintMap, withTaintMap,
enterBlockScope, leaveBlockScope, enterFunctionScope, leaveFunctionScope
)
where
import Prelude hiding (log)

import EntropicDependency
import InstrumentationState
import TaintMap

import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad hiding (enterBlockScope, leaveBlockScope, enterFunctionScope, leaveFunctionScope)
import qualified Language.C.Analysis.TravMonad as ST
import Language.C.Pretty

import qualified Data.Map as Map
import Data.Maybe

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
addToTaints id t = modifyUserState $ \is -> is { taints = defLocal (taints is) id t }

getTaintValue :: Ident -> InstTrav EntropicDependency
getTaintValue id = do
    st <- getUserState
    let tv = fromMaybe NoDependency $ lookupName (taints st) id 
    return tv

getTaintMap :: InstTrav TaintMap
getTaintMap = taints <$> getUserState

setTaintMap :: TaintMap -> InstTrav ()
setTaintMap tm = modifyUserState (\st -> st { taints = tm })

withTaintMap :: (TaintMap -> TaintMap) -> InstTrav ()
withTaintMap f = getTaintMap >>= setTaintMap . f

-- scope manipulation

enterNewTaintScope :: InstTrav ()
enterNewTaintScope = withTaintMap enterNewScope

leaveTaintScope :: InstTrav ()
leaveTaintScope = withTaintMap (fst . leaveScope)

enterBlockScope :: InstTrav ()
enterBlockScope = enterNewTaintScope >> ST.enterBlockScope

leaveBlockScope :: InstTrav ()
leaveBlockScope = leaveTaintScope >> ST.leaveBlockScope

enterFunctionScope :: InstTrav ()
enterFunctionScope = enterNewTaintScope >> ST.enterFunctionScope

leaveFunctionScope :: InstTrav ()
leaveFunctionScope = leaveTaintScope >> ST.leaveFunctionScope