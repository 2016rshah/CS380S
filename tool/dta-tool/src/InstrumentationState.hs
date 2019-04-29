module InstrumentationState(
InstrumentationState, 
    taints,
    preservingFns,
    nonPreservingFns,
addToLog, addPrettyToLog,
emptyInstState
)
where
import EntropicDependency
import TaintMap

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
        preservingFns :: [Decl],
        nonPreservingFns :: [Decl]
    }

instance Show InstrumentationState where
    show is = "Log: " ++ notes is ++ "\n\n" ++
                "Taint values: " ++ show (Map.mapKeys (show . pretty) (globalTaints (taints is))) 

addPrettyToLog :: (Pretty a) => a -> InstrumentationState -> InstrumentationState
addPrettyToLog o is = let lg = notes is ++ show (pretty o)  ++ "\n" in is { notes = lg }

addToLog :: (Show a) => a -> InstrumentationState -> InstrumentationState
addToLog o is = let lg = notes is ++ show o ++ "\n" in is { notes = lg }

emptyInstState :: InstrumentationState
emptyInstState = IState { 
                    notes = "", 
                    taints = emptyTaintMap,
                    preservingFns = [],
                    nonPreservingFns = []
                    }