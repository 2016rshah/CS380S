module TaintMap(
TaintMap,
taintMap, 
globalTaints,
mergeTaintMap,
prettyTaintMap,
enterNewScope, leaveScope,
defGlobal, updateGlobalWith,
defLocal, updateLocal,
updateLocalWith,
lookupTaint
)
where
import Utils
import EntropicDependency

import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Ident

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Control.Arrow

data TaintMap = TaintMap (Map.Map Ident EntropicDependency) [Map.Map Ident EntropicDependency]

taintMap :: TaintMap
taintMap = TaintMap Map.empty []

prettyTaintMap :: TaintMap -> [(String, EntropicDependency)]
prettyTaintMap (TaintMap gt []) = map (first identToString) $ Map.toList gt
prettyTaintMap (TaintMap gt (l:lt)) = map (first identToString) (Map.toList l) ++ prettyTaintMap (TaintMap gt lt)

globalTaints (TaintMap gt _) = gt

localTaints (TaintMap _ lt) = lt

hasLocalNames (TaintMap _ lt) = not $ null lt

defGlobal :: TaintMap -> Ident -> EntropicDependency -> TaintMap
defGlobal (TaintMap gt lt) id dep = TaintMap (Map.insert id dep gt) lt

enterNewScope :: TaintMap -> TaintMap
enterNewScope (TaintMap gt lt) = TaintMap gt (Map.empty:lt)

leaveScope :: TaintMap -> (TaintMap, Map.Map Ident EntropicDependency)
leaveScope (TaintMap gt []) = error "TaintMap.leaveScope: no local scope!"
leaveScope (TaintMap gt (l:lt)) = (TaintMap gt lt, l)

defLocal :: TaintMap -> Ident -> EntropicDependency -> TaintMap
defLocal tm@(TaintMap gt []) id dep = defGlobal tm id dep
defLocal (TaintMap gt (l:lt)) id dep = let l' = Map.insert id dep l in TaintMap gt (l':lt)

updateLocal :: TaintMap -> Ident -> EntropicDependency -> (Maybe EntropicDependency, TaintMap)
updateLocal = updateLocalWith (\v _ -> v) 

updateGlobalWith :: EntropyCombinator -> TaintMap -> Ident -> EntropicDependency -> (Maybe EntropicDependency, TaintMap)
updateGlobalWith comb (TaintMap gt lt) ident dep = let (result, gt') = updateMapWith comb ident dep gt
                                                   in (result, TaintMap gt' lt)

updateLocalWith :: EntropyCombinator -> TaintMap -> Ident -> EntropicDependency -> (Maybe EntropicDependency, TaintMap)
updateLocalWith comb tm@(TaintMap gt []) ident dep = updateGlobalWith comb tm ident dep
updateLocalWith comb (TaintMap gt (l:lt)) ident dep = let (result, l') = updateMapWith comb ident dep l
                                                    in case result of
                                                        Just _ -> (result, TaintMap gt (l':lt))
                                                        Nothing -> updateLocalWith comb (TaintMap gt lt) ident dep

updateMapWith :: (Ord k) => (v -> v -> v) -> k -> v -> Map.Map k v -> (Maybe v, Map.Map k v)
updateMapWith f = Map.insertLookupWithKey (const f)

lookupTaint :: TaintMap -> Ident -> Maybe EntropicDependency
lookupTaint tm id = fst $ updateLocalWith const tm id NoDependency

mergeTaintMap :: EntropyCombinator -> TaintMap -> TaintMap -> TaintMap
mergeTaintMap comb (TaintMap gt1 lt1) (TaintMap gt2 lt2) =
    let gt = Map.unionWith comb gt1 gt2
        lt = zipWith (Map.unionWith comb) lt1 lt2
    in TaintMap gt lt
