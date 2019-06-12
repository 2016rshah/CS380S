module ProductProgram(
productProgram
)
where

import Utils
import VarRemap

import Language.C.Syntax.AST
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.DefTable
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Data.Node
import Language.C.Analysis.Export
import Language.C.Analysis.TypeUtils

import Data.Maybe
import Data.Either
import qualified Data.Map as Map
import Control.Arrow

productProgram :: String -> CTranslUnit -> CTranslUnit -> IO CTranslUnit
productProgram fnName ast1 ast2 
    | fnName == "main" = error "Cannot take the product of `main`"
    | otherwise =
        let 
            ast1' = remapProgram FirstVersion ast1
            ast2' = remapProgram SecondVersion ast2
            (g1, s1) = runTravOrDie () $ analyseAST ast1'
            (g2, s2) = runTravOrDie () $ analyseAST ast2'
            fId = mkIdent nopos fnName (Name 0)
            (fDef1, g1') = extractFunction g1 $ fnName ++ versionSuffix FirstVersion
            (fDef2, g2') = extractFunction g2 $ fnName ++ versionSuffix SecondVersion
            (main1, g1'') = extractFunction g1' "main"
            (main2, g2'') = extractFunction g2' "main"
            mainId = mkIdent nopos "main" (Name 1)
            mainDecl = FunctionDef main2
            g = mergeGlobalDecls g1'' g2''
            g' = g { gObjs = Map.insert mainId mainDecl (gObjs g) } -- use the second version of main
            f = prodProg fnName fDef1 fDef2
            funDecl = FunctionDef f
        in do
            return $ export $ g' { gObjs = Map.insert fId funDecl (gObjs g') }

prodProg :: String -> FunDef -> FunDef -> FunDef
prodProg fnName p1 p2
    | not (compatibilityCheck p1 p2) = error "Attempted to take product of incompatible functions"
    | otherwise =
        let (FunDef (VarDecl varName1 declAttrs1 fType1) body1 node1) = p1
            (FunDef (VarDecl varName2 declAttrs2 fType2) body2 node2) = p2
            (params1, isVar1) = getFnParams p1
            (params2, isVar2) = getFnParams p2
            retType = getReturnType p1
            varName = VarName (mkIdent nopos (fnName ++ versionSuffix SecondVersion) (Name 0)) Nothing
            declAttrs = declAttrs1
            fType = FunType retType params1 (isVar1 || isVar2)
            funType = FunctionType fType noAttributes -- TODO: place in attributes from p1 or p2
            varDecl = VarDecl varName declAttrs funType
            (CCompound localLabels bodyItems node) = head $ prodStmts body1 body2
            paramDecls = CBlockDecl <$> makeParamDecls params1 params2
            body = CCompound localLabels (paramDecls ++ bodyItems) node
        in FunDef varDecl body node1

-- this function initializes the parameters of the second version of the
-- productized program to be the parameters of the first version
makeParamDecls :: [ParamDecl] -> [ParamDecl] -> [CDecl]
makeParamDecls = zipWith f
    where 
        f pd1 pd2@(ParamDecl (VarDecl vName (DeclAttrs _ _ attrs) ty) node) = 
            let id1 = declIdent pd1
                id2 = declIdent pd2
                (declspecs, declr) = exportDeclr [] ty attrs vName
                initExpr = CVar id1 undefNode
                init = CInitExpr initExpr undefNode
            in CDecl declspecs [(Just declr, Just init, Nothing)] undefNode
        f _ _ = error "Abstract parameters in producted function"

prodBlocks :: [CBlockItem] -> [CBlockItem] -> [CBlockItem]
prodBlocks bs1 [] = bs1
prodBlocks [] bs2 = bs2
prodBlocks blocks1@(b1:bs1) blocks2@(b2:bs2)
    | isBlockAtom b1 = b1:(prodBlocks blocks2 bs1) -- interleave atoms for as long as possible
    | isBlockAtom b2 = b2:(prodBlocks blocks1 bs2)
    | (CBlockStmt stmt1) <- b1
    , (CBlockStmt stmt2) <- b2
    =
        let stmts = CBlockStmt <$> prodStmts stmt1 stmt2
            blocks = prodBlocks bs1 bs2
        in stmts ++ blocks

prodStmts :: CStat -> CStat -> [CStat]
prodStmts (CCompound _ blocks1 node) (CCompound _ blocks2 _) = [CCompound [] (prodBlocks blocks1 blocks2) node]
prodStmts (CIf ifExpr thenStmt mElse node) stmt2 =
    let s1 = flattenStmt $ prodStmts thenStmt stmt2
        s2 = flattenStmt <$> prodStmts stmt2 <$> mElse
    in [CIf ifExpr s1 s2 node]
prodStmts (CWhile guard1 stmt1 isDoWhile1 node1) (CWhile guard2 stmt2 isDoWhile2 node2) =
    let 
        s1 = if isDoWhile1 then [stmt1] else []
        s2 = if isDoWhile2 then [stmt2] else []
        while0 = CWhile (CBinary CLndOp guard1 guard2 undefNode) (flattenStmt $ prodStmts stmt1 stmt2) False undefNode
        while1 = CWhile guard1 stmt1 False undefNode
        while2 = CWhile guard2 stmt2 False undefNode
    in s1 ++ s2 ++ [while0, while1, while2]
prodStmts (CFor init expr2 expr3 stmt _) (CFor init' expr2' expr3' stmt' _) =
    let
        preDecls = CBlockDecl <$> rights [init, init']
        newInit = let
                    initExprs = catMaybes $ lefts [init, init']
                  in
                    if length initExprs == 0
                    then Left Nothing
                    else if length initExprs == 1
                    then Left $ Just $ head initExprs
                    else Left $ Just $ CComma initExprs undefNode 
        newExpr2 = let
                    expr2s = catMaybes [expr2, expr2']
                   in
                    if length expr2s == 0
                    then Nothing
                    else if length expr2s == 1
                    then Just $ head expr2s
                    else Just $ CBinary CLndOp (expr2s !! 0) (expr2s !! 1) undefNode
        newExpr3 = let
                    expr3s = catMaybes [expr3, expr3']
                   in
                    if length expr3s == 0
                    then Nothing
                    else if length expr3s == 1
                    then Just $ head expr3s
                    else Just $ CComma expr3s undefNode
        newStmt = flattenStmt $ prodStmts stmt stmt'
        for0 = CFor newInit newExpr2 newExpr3 newStmt undefNode
        for1 = CFor (Left Nothing) expr2 expr3 stmt undefNode
        for2 = CFor (Left Nothing) expr2' expr3' stmt' undefNode
        fors = CBlockStmt <$> [for0, for1, for2]
    in [CCompound [] (preDecls ++ fors) undefNode]
prodStmts s1 s2 = [s1, s2]

flattenStmt :: [CStat] -> CStat
flattenStmt stmts
    | length stmts == 1 = head stmts
    | otherwise = CCompound [] (CBlockStmt <$> stmts) undefNode

isBlockAtom :: CBlockItem -> Bool
isBlockAtom (CNestedFunDef _) = True
isBlockAtom (CBlockDecl _) = True
isBlockAtom (CBlockStmt stmt) = isAtom stmt

isAtom :: CStat -> Bool
isAtom (CIf _ _ _ _) = False
isAtom (CWhile _ _ _ _) = False
isAtom (CFor _ _ _ _ _) = False
isAtom (CCompound _ _ _) = False
isAtom _ = True

compatibilityCheck :: FunDef -> FunDef -> Bool
compatibilityCheck p1 p2 =
    let ty1 = declType p1
        ty2 = declType p2
    in sameType ty1 ty2

extractFunction :: GlobalDecls -> String -> (FunDef, GlobalDecls)
extractFunction decls fnName = 
    let fId = mkIdent nopos fnName (Name 0)
        f (Just (FunctionDef fundef)) = fundef
        f _ = error "Could not extract function"
        g objs = decls { gObjs = objs }
        (funDecl, objs') = Map.updateLookupWithKey (const . const Nothing) fId $ gObjs decls
    in (f funDecl, g objs')