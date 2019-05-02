module Utils(
makeStrConst, setStrConst,
identOfDecl, identOfExpr,
resultOrDie, runTravOrDie, runTravOrDie_,
getFunDef, emptyFunDef,
filterBuiltIns
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Analysis.TravMonad
import Language.C.Analysis.DefTable
import Language.C.Analysis.SemRep

import qualified Data.List as List
import qualified Data.Map as Map

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

resultOrDie :: (Show a) => Either a b -> b
resultOrDie (Left err) = error $ show err
resultOrDie (Right x) = x

runTravOrDie s = resultOrDie . (runTrav s)
runTravOrDie_ s = fst . (runTravOrDie s)

emptyFunDef :: String -> FunDef
emptyFunDef fnName =
    let varname = VarName (mkIdent nopos fnName (Name 0)) Nothing
        declAttrs = DeclAttrs noFunctionAttrs NoStorage noAttributes
        vardecl = VarDecl varname declAttrs funtype
        returnType = DirectType TyVoid noTypeQuals noAttributes
        funtype = FunctionType (FunType returnType [] False) noAttributes
        stmt = CCompound [] [CBlockStmt (CReturn Nothing node)] node
        node = undefNode
    in FunDef vardecl stmt node

filterBuiltIns = Map.filterWithKey noBuiltIns

noBuiltIns idn _ = let n = identToString idn
                   in not ("__builtin" `List.isPrefixOf` n) &&
                       (n /= "__FUNCTION__") &&
                       (n /= "__PRETTY_FUNCTION__") &&
                       (n /= "__func__" )

getFunDef :: DefTable -> Ident -> Maybe FunDef
getFunDef dt ident = let funDecl = Map.lookup ident $ gObjs $ globalDefs dt
                     in funDecl >>= funDefFromIdentDecl
    where funDefFromIdentDecl (FunctionDef fundef) = Just fundef
          funDefFromIdentDecl _ = Nothing