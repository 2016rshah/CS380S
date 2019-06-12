{-# LANGUAGE FlexibleInstances #-}

module InstrumentationTrav(
InstTrav,
addPreservingFn, addNonPreservingFn,
getPreservingFns, getNonPreservingFns,
addToTaints, updateTaint, getTaintValue, getTaintMap, setTaintMap, withTaintMap,
getTaintEnv, updateTaintEnv,
enterBlockScope, leaveBlockScope, enterFunctionScope, leaveFunctionScope,
)
where
import Prelude hiding (log)

import EntropicDependency
import InstrumentationState
import TaintMap
import TaintEnv
import Loggable

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

instance Loggable InstTrav where
    log = modifyUserState . addToLog
    logPretty = modifyUserState . addPrettyToLog

getPreservingFns :: InstTrav [Decl]
getPreservingFns = preservingFns <$> getUserState

getNonPreservingFns :: InstTrav [Decl]
getNonPreservingFns = nonPreservingFns <$> getUserState

addPreservingFn :: Decl -> InstTrav ()
addPreservingFn f = modifyUserState $ \is -> is { preservingFns = preservingFns is ++ [f] }

addNonPreservingFn :: Decl -> InstTrav ()
addNonPreservingFn f = modifyUserState $ \is -> is { nonPreservingFns = nonPreservingFns is ++ [f] }

addToTaints :: Ident -> EntropicDependency -> InstTrav ()
addToTaints id t = modifyUserState $ \is -> is { taints = defLocal (taints is) id t }

updateTaint :: Ident -> EntropicDependency -> InstTrav ()
updateTaint id t = modifyUserState $ \is -> is { taints = snd (updateLocal (taints is) id t) }

getTaintValue :: Ident -> InstTrav EntropicDependency
getTaintValue id = do
    st <- getUserState
    let tv = fromMaybe NoDependency $ lookupTaint (taints st) id 
    return tv

getTaintMap :: InstTrav TaintMap
getTaintMap = taints <$> getUserState

setTaintMap :: TaintMap -> InstTrav ()
setTaintMap tm = modifyUserState (\st -> st { taints = tm })

withTaintMap :: (TaintMap -> TaintMap) -> InstTrav ()
withTaintMap f = getTaintMap >>= setTaintMap . f

getTaintEnv :: InstTrav TaintEnv 
getTaintEnv = taintEnv <$> getUserState

updateTaintEnv :: String -> TaintTree -> InstTrav ()
updateTaintEnv fnName tTree = modifyUserState (\st -> st { taintEnv = Map.insert fnName tTree (taintEnv st) })

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