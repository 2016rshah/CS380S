module Utils(
makeStrConst, setStrConst,
identOfDecl, identOfExpr,
resultOrDie, runTravOrDie,
getFunDef
)
where
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Data.Ident
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

getFunDef :: DefTable -> Ident -> Maybe CFunDef
getFunDef dt ident = let funDecl = lookup ident $ gObjs $ globalDefs dt
                     in funDecl >>= funDefFromIdentDecl
    where funDefFromIdentDecl (IdentDecl (FunctionDef fundef)) = Just fundef
          funDefFromIdentDecl _ = Nothing