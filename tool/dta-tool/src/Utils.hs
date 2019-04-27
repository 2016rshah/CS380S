module Utils(
makeStrConst, setStrConst,
identOfDecl, identOfExpr,
mergeNameSpaceWith,
prettyTaintMap
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Analysis.NameSpaceMap

import qualified Data.List as List
import qualified Data.Map as Map
import Control.Arrow

makeStrConst :: String -> CExpr -> CExpr
makeStrConst strConst expr = let node_info = nodeInfo expr 
                                in CConst $ CStrConst (cString strConst) node_info

setStrConst :: String -> CDecl -> CDecl
setStrConst strConst (CDecl typeSpecifier [(Just declr, _, expr)] node_info) =
    -- TODO: node info needs fixing? I.E. not just inherited
    let initlr = CInitExpr (CConst (CStrConst (cString strConst) node_info)) node_info 
        typeSpecifier' = CTypeSpec (CCharType node_info)
        CDeclr id _ _ _ node_info2 = declr
        declr' = CDeclr id [CPtrDeclr [] node_info2] Nothing [] node_info2
    in CDecl [typeSpecifier'] [(Just declr', Just initlr, expr)] node_info
setStrConst _ _ = error "Tried to set str const to invalid declaration"

identOfDecl :: CDecl -> Maybe Ident
identOfDecl (CDecl _ declr _) = case declr of
                                    [(Just (CDeclr (Just id) _ _ _ _), _, _)] -> Just id
                                    _ -> Nothing

identOfExpr :: CExpr -> Maybe Ident
identOfExpr (CVar id _) = Just id
identOfExpr (CUnary CIndOp expr _) = identOfExpr expr
identOfExpr (CIndex arr _ _) = identOfExpr arr
identOfExpr _ = Nothing

prettyTaintMap nsm =  map (first identToString) $ nsMapToList nsm

-- due to the fact that the Language.C interface doesn't expose the NameSpaceMap constructor, 
-- we have to rewrite this functionality
mergeNameSpaceWith :: (Ord k) => (a -> a -> a) -> NameSpaceMap k a -> NameSpaceMap k a -> NameSpaceMap k a
mergeNameSpaceWith mergeFn nsmap1 nsmap2 =
    let global1 = globalNames nsmap1
        global2 = globalNames nsmap2
        local1 = localNames nsmap1
        local2 = localNames nsmap2
        global = Map.unionWith mergeFn global1 global2
        local = zipWith localUnion local1 local2
        nsm = addGlobals nameSpaceMap global
        nsm' = addLocals nsm local
    in addLocals nsm' local
    where 
        localUnion [] ls2 = ls2
        localUnion ls1 [] = ls1
        localUnion l1 l2 =
            let m1 = Map.fromList l1
                m2 = Map.fromList l2
                m = Map.unionWith mergeFn m1 m2
            in Map.assocs m

        addGlobals :: (Ord k) => NameSpaceMap k v -> Map.Map k v -> NameSpaceMap k v
        addGlobals nsm globals = 
            let kvs = Map.assocs globals
                f (k,v) m = fst $ defGlobal m k v 
            in foldr f nsm kvs
        
        addLocals :: (Ord k) => NameSpaceMap k v -> [[(k,v)]] -> NameSpaceMap k v
        addLocals nsm [] = nsm
        addLocals nsm (l:ls) = 
            let nsm' = enterNewScope nsm
                f (k,v) m = fst $ defLocal m k v
            in addLocals (foldr f nsm' l) ls
    