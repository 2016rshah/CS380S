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

import qualified Data.Map as Map
import Control.Arrow

productProgram :: String -> CTranslUnit -> CTranslUnit -> IO CTranslUnit
productProgram fnName ast1 ast2 =
    let 
        ast1' = remapProgram FirstVersion ast1
        ast2' = remapProgram SecondVersion ast2
        (g1, s1) = runTravOrDie () $ analyseAST ast1'
        (g2, s2) = runTravOrDie () $ analyseAST ast2'
        fId = mkIdent nopos fnName (Name 0)
        (fDef1, g1') = extractFunction g1 $ fnName ++ versionSuffix FirstVersion
        (fDef2, g2') = extractFunction g2 $ fnName ++ versionSuffix SecondVersion
        g = mergeGlobalDecls g1' g2'
        f = prodProg fnName fDef1 fDef2
        funDecl = FunctionDef f
    in do
        return $ export $ g { gObjs = Map.insert fId funDecl (gObjs g) }

prodProg :: String -> FunDef -> FunDef -> FunDef
prodProg fnName p1 p2
    | not (compatibilityCheck p1 p2) = error "Attempted to take product of incompatible functions"
    | otherwise =
        let (FunDef (VarDecl varName1 declAttrs1 fType1) body1 node1) = p1
            (FunDef (VarDecl varName2 declAttrs2 fType2) body2 node2) = p2
            (params1, isVar1) = getFnParams p1
            (params2, isVar2) = getFnParams p2
            retType = getReturnType p1
            varName = VarName (mkIdent nopos fnName (Name 0)) Nothing
            declAttrs = declAttrs1
            fType = FunType retType (params1 ++ params2) (isVar1 || isVar2)
            funType = FunctionType fType noAttributes -- TODO: place in attributes from p1 or p2
            varDecl = VarDecl varName declAttrs funType
            body = head $ prodStmts body1 body2
        in FunDef varDecl body node1

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
isAtom (CWhile _ _ _ _) = True
isAtom (CCompound _ _ _) = True
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