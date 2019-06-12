module TaintEnv(
TaintEnv,
TaintVal,
TaintTree(..),
emptyTaintEnv, getTaintVal,
entropicDepToTaintVal
)
where

import Language.C.Syntax.AST
import qualified Data.Map as Map

import EntropicDependency

type TaintVal = Bool

type TaintEnv = Map.Map String TaintTree 

emptyTaintEnv = Map.empty

data TaintTree = 
    CompoundTaint TaintVal [TaintTree] |
    IfTaint TaintVal TaintTree (Maybe TaintTree) |
    AtomTaint TaintVal
    deriving (Show)

entropicDepToTaintVal :: EntropicDependency -> TaintVal
entropicDepToTaintVal Source = True
entropicDepToTaintVal HighEntropy = True
entropicDepToTaintVal LowEntropy = False
entropicDepToTaintVal NoDependency = False

getTaintVal :: TaintTree -> TaintVal
getTaintVal (CompoundTaint tv _) = tv
getTaintVal (IfTaint tv _ _) = tv
getTaintVal (AtomTaint tv) = tv
