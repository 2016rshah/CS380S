module EntropicDependency(
Taints, OpDependency(..), EntropicDependency(..),
applyOpDep,
assignmentOpDependency, unaryOpDependency, binaryOpDependency, functionCallDependency
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Ident

import Data.Maybe
import Data.List
import qualified Data.Map as Map

type Taints = Map.Map Ident EntropicDependency

data OpDependency =
    Preserving |
    Nonpreserving deriving (Show, Eq)

data EntropicDependency =
    Source |
    HighEntropy |
    LowEntropy |
    NoDependency deriving (Show, Eq)

instance Ord EntropicDependency where
    compare x y = fromJust compared
        where ordering = [LowEntropy, NoDependency, HighEntropy, Source]
              compared = do
                i <- elemIndex x ordering
                j <- elemIndex y ordering
                return $ compare i j

checkPreserving :: (Eq a) => [a] -> a -> OpDependency
checkPreserving preservingOps op = if op `elem` preservingOps then Preserving else Nonpreserving

applyOpDependency :: OpDependency -> [EntropicDependency] -> EntropicDependency
applyOpDependency opDep deps = 
    let combined = foldr combineDependencies NoDependency deps in
    case combined of
        NoDependency -> NoDependency
        _ -> case opDep of
                Preserving -> combined
                Nonpreserving -> LowEntropy

applyOpDep opDep dep = applyOpDependency opDep [dep]

combineDependencies :: EntropicDependency -> EntropicDependency -> EntropicDependency
combineDependencies Source dep = 
    case dep of
        Source -> Source
        HighEntropy -> HighEntropy
        LowEntropy -> LowEntropy
        NoDependency -> HighEntropy
combineDependencies HighEntropy dep = 
    case dep of
        HighEntropy -> HighEntropy
        LowEntropy -> LowEntropy
        NoDependency -> HighEntropy
        _ -> combineDependencies dep HighEntropy
combineDependencies LowEntropy dep = LowEntropy
combineDependencies NoDependency dep =
    case dep of
        NoDependency -> NoDependency
        _ -> combineDependencies dep NoDependency

assignmentOpDependency :: CAssignOp -> EntropicDependency -> EntropicDependency
assignmentOpDependency op = applyOpDep $ checkPreserving [CAssignOp, CXorAssOp, CAddAssOp, CMulAssOp, CSubAssOp] op

unaryOpDependency :: CUnaryOp -> EntropicDependency -> EntropicDependency
unaryOpDependency op = applyOpDep $
    checkPreserving [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp, CPlusOp, CMinOp, CCompOp, CNegOp, CIndOp] op

binaryOpDependency :: CBinaryOp -> EntropicDependency -> EntropicDependency -> EntropicDependency
binaryOpDependency op expr1 expr2 = applyOpDependency (checkPreserving [CMulOp, CAddOp, CSubOp, CXorOp] op) [expr1, expr2]

functionCallDependency :: String -> [EntropicDependency] -> EntropicDependency
functionCallDependency fnName = applyOpDependency (checkPreserving ["f"] fnName)
