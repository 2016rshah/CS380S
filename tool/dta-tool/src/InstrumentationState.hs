module InstrumentationState(
InstrumentationState(..),
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

type Log = [String]

data InstrumentationState = IState 
    { 
        notes :: Log, 
        taints :: TaintMap,
        transformedFns :: [(FunDef, FunDef)],
        preservingFns :: [Decl],
        nonPreservingFns :: [Decl],
        ast :: CTranslUnit
    }

instance Show InstrumentationState where
    show is = "Log: " ++ show (notes is) ++ "\n\n" ++
                "Taint values: " ++ show (Map.mapKeys (show . pretty) (globalTaints (taints is))) ++ "\n\n" ++
                "Transformed Functions: \n" ++ showFns (transformedFns is)
        where showFns = concatMap (\(f, f') -> "\t" ++ (show . pretty) f ++ " -> " ++ (show . pretty) f' ++ "\n")

addPrettyToLog :: (Pretty a) => a -> InstrumentationState -> InstrumentationState
addPrettyToLog o is = let lg = notes is ++ [show (pretty o)] in is { notes = lg }

addToLog :: (Show a) => a -> InstrumentationState -> InstrumentationState
addToLog o is = let lg = notes is ++ [show o] in is { notes = lg }

emptyInstState :: String -> InstrumentationState
emptyInstState fileName = IState { 
                                    notes = [], 
                                    ast = CTranslUnit [] (OnlyPos pos (pos, 0)),
                                    transformedFns = [],
                                    taints = taintMap,
                                    preservingFns = [],
                                    nonPreservingFns = []
                                    }
    where pos = initPos fileName 