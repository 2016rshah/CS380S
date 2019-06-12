module InstrumentationState(
InstrumentationState, 
    taints,
    taintEnv,
    preservingFns,
    nonPreservingFns,
addToLog, addPrettyToLog,
emptyInstState
)
where
import EntropicDependency
import TaintMap
import TaintEnv

import Language.C.Data.Position
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Syntax.AST
import Language.C.Analysis.SemRep
import Language.C.Analysis.AstAnalysis
import Language.C.Pretty

import qualified Data.Map as Map

data InstrumentationState = IState 
    { 
        notes :: String, 
        taints :: TaintMap,
        taintEnv :: TaintEnv,
        preservingFns :: [Decl],
        nonPreservingFns :: [Decl]
    }

instance Show InstrumentationState where
    show is = "Log: " ++ notes is ++ "\n\n" ++
                "Taint values: " ++ show (Map.mapKeys (show . pretty) (globalTaints (taints is))) ++
                "Taint tree: " ++ show (taintEnv is)

addPrettyToLog :: (Pretty a) => a -> InstrumentationState -> InstrumentationState
addPrettyToLog o is = let lg = notes is ++ show (pretty o)  ++ "\n" in is { notes = lg }

addToLog :: (Show a) => a -> InstrumentationState -> InstrumentationState
addToLog o is = let lg = notes is ++ show o ++ "\n" in is { notes = lg }

emptyInstState :: InstrumentationState
emptyInstState = IState { 
                    notes = "", 
                    taints = emptyTaintMap,
                    taintEnv = emptyTaintEnv,
                    preservingFns = [],
                    nonPreservingFns = []
                    }