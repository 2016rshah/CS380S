module VarRemap(
ProgramVersion(..),
remapProgram,
versionSuffix
)
where
import Prelude hiding (log)

import Utils
import Loggable

import Language.C.Syntax.AST
import Language.C.Syntax.Utils
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Analysis.TravMonad
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.NameSpaceMap
import Language.C.Analysis.SemRep
import Language.C.Analysis.DefTable hiding (enterFunctionScope, leaveFunctionScope,
                                            enterBlockScope, leaveBlockScope)
import Language.C.Pretty

import qualified Data.Map as Map
import Data.Maybe
import Control.Monad

data ProgramVersion = FirstVersion | SecondVersion

versionSuffix :: ProgramVersion -> String
versionSuffix FirstVersion = "_1"
versionSuffix SecondVersion = "_2"

remapProgram :: ProgramVersion -> CTranslUnit -> CTranslUnit
remapProgram version (CTranslUnit decls node) = 
    let decls' = map (remapExtDecl version) decls
    in CTranslUnit decls' node

remapExtDecl :: ProgramVersion -> CExtDecl -> CExtDecl
remapExtDecl version (CDeclExt decl) = CDeclExt $ remapDecl version decl
remapExtDecl version (CFDefExt fundef) = CFDefExt $ remapFun version fundef
remapExtDecl version asm = asm

remapDecl :: ProgramVersion -> CDecl -> CDecl
remapDecl version (CDecl declspecs declrs node) =
    let declrs' = map remapDecl' declrs
    in CDecl declspecs declrs' node
    where
        remapDecl' (decl, init, expr) =
            let decl' = remapDeclr version <$> decl
                init' = remapInitializer version <$> init
                expr' = remapExpr version <$> expr
            in (decl', init', expr')
remapDecl version (CStaticAssert expr strLit node) =
    let expr' = remapExpr version expr
    in CStaticAssert expr' strLit node

remapInitializer :: ProgramVersion -> CInit -> CInit
remapInitializer version (CInitExpr expr node) = CInitExpr (remapExpr version expr) node
remapInitializer version (CInitList [] node) = CInitList [] node
remapInitializer version (CInitList xs node) = CInitList (remapInitList version xs) node

remapInitList :: ProgramVersion -> CInitList -> CInitList
remapInitList version [] = []
remapInitList version ((desigs, init):xs) = 
    let xs' = remapInitList version xs
        desigs' = map (remapDesig version) desigs
        init' = remapInitializer version init
        x' = (desigs', init')
    in x':xs'

remapDesig :: ProgramVersion -> CDesignator -> CDesignator
remapDesig version (CArrDesig expr node) = CArrDesig (remapExpr version expr) node
remapDesig version (CMemberDesig id node) = CMemberDesig (remapIdent version id) node
remapDesig version (CRangeDesig expr1 expr2 node) = CRangeDesig (remapExpr version expr1) (remapExpr version expr2) node

remapIdent :: ProgramVersion -> Ident -> Ident
remapIdent version id = 
    let suff = versionSuffix version
    in appendToId suff id

remapFun :: ProgramVersion -> CFunDef -> CFunDef
remapFun version (CFunDef declspecs declr oldstyle_decls stmt node_info) =
    let declr' = if isMain declr then declr else remapDeclr version declr
        stmt' = remapStmt version stmt
    in CFunDef declspecs declr' oldstyle_decls stmt' node_info

remapDeclr :: ProgramVersion -> CDeclr -> CDeclr
remapDeclr version declr@(CDeclr maybeId derivedDeclrs maybeStrLit attrs node) =
    let id' = remapIdent version <$> maybeId
        derivedDeclrs' = map (remapDerivedDeclr version) derivedDeclrs
    in CDeclr id' derivedDeclrs' maybeStrLit attrs node

remapDerivedDeclr :: ProgramVersion -> CDerivedDeclr -> CDerivedDeclr
remapDerivedDeclr version (CFunDeclr params attrs node) =
    let params' = case params of 
                    Left idents -> Left $ map (remapIdent version) idents
                    Right (decls, isVariadic) -> Right (map (remapDecl version) decls, isVariadic)
    in CFunDeclr params' attrs node
remapDerivedDeclr version derDeclr = derDeclr

remapStmt :: ProgramVersion -> CStat -> CStat
remapStmt version (CCompound idents blocks node) =
    let idents' = remapIdent version <$> idents
        blocks' = remapBlockItem version <$> blocks
    in CCompound idents' blocks' node
remapStmt version (CReturn mExpr node) = CReturn (remapExpr version <$> mExpr) node
remapStmt version (CLabel id stmt attrs node) = CLabel (remapIdent version id) (remapStmt version stmt) (remapAttr version <$> attrs) node
remapStmt version (CCase expr stmt node) = CCase (remapExpr version expr) (remapStmt version stmt) node
remapStmt version (CCases lower upper stmt node) = CCases (remapExpr version lower) (remapExpr version upper) (remapStmt version stmt) node
remapStmt version (CDefault stmt node) = CDefault (remapStmt version stmt) node
remapStmt version (CExpr mExpr node) = CExpr (remapExpr version <$> mExpr) node
remapStmt version (CIf ifExpr thenStmt maybeElseStmt node) = 
    let ifExpr' = remapExpr version ifExpr
        thenStmt' = remapStmt version thenStmt
        maybeElseStmt' = remapStmt version <$> maybeElseStmt
    in CIf ifExpr' thenStmt' maybeElseStmt' node
remapStmt version (CSwitch selector switchStmt node) = CSwitch (remapExpr version selector) (remapStmt version switchStmt) node
remapStmt version (CWhile guard stmt isDoWhile node) = CWhile (remapExpr version guard) (remapStmt version stmt) isDoWhile node
remapStmt version (CFor init expr2 expr3 stmt node) =
    let init' = case init of
                    Left mInitExpr -> Left $ remapExpr version <$> mInitExpr
                    Right decl -> Right $ remapDecl version decl
        expr2' = remapExpr version <$> expr2
        expr3' = remapExpr version <$> expr3
        stmt' = remapStmt version stmt
    in CFor init' expr2' expr3' stmt' node
remapStmt version (CGoto id node) = CGoto (remapIdent version id) node
remapStmt version (CGotoPtr expr node) = CGotoPtr (remapExpr version expr) node
remapStmt _ c = c

remapAttr :: ProgramVersion -> CAttr -> CAttr
remapAttr version (CAttr id exprs node) = CAttr (remapIdent version id) (remapExpr version <$> exprs) node

remapBlockItem :: ProgramVersion -> CBlockItem -> CBlockItem
remapBlockItem version (CBlockStmt stmt) = CBlockStmt $ remapStmt version stmt
remapBlockItem version (CBlockDecl decl) = CBlockDecl $ remapDecl version decl
remapBlockItem version (CNestedFunDef fundef) = CNestedFunDef $ remapFun version fundef

remapExpr :: ProgramVersion -> CExpr -> CExpr
remapExpr version (CVar id node) = CVar (remapIdent version id) node
remapExpr version (CComma exprs node) = CComma (remapExpr version <$> exprs) node
remapExpr version (CAssign op lhs rhs node) = CAssign op (remapExpr version lhs) (remapExpr version rhs) node
remapExpr version (CCond expr1 mExpr2 expr3 node) = 
    CCond (remapExpr version expr1) (remapExpr version <$> mExpr2) (remapExpr version expr3) node
remapExpr version (CBinary op expr1 expr2 node) = CBinary op (remapExpr version expr1) (remapExpr version expr2) node
remapExpr version (CCast decl expr node) = CCast (remapDecl version decl) (remapExpr version expr) node
remapExpr version (CUnary op expr node) = CUnary op (remapExpr version expr) node
remapExpr version (CSizeofExpr expr node) = CSizeofExpr (remapExpr version expr) node
remapExpr version (CSizeofType decl node) = CSizeofType (remapDecl version decl) node
remapExpr version (CAlignofExpr expr node) = CAlignofExpr (remapExpr version expr) node
remapExpr version (CAlignofType decl node) = CAlignofType (remapDecl version decl) node
remapExpr version (CComplexReal expr node) = CComplexReal (remapExpr version expr) node
remapExpr version (CComplexImag expr node) = CComplexImag (remapExpr version expr) node
remapExpr version (CIndex expr1 expr2 node) = CIndex (remapExpr version expr1) (remapExpr version expr2) node
remapExpr version (CCall expr1 exprs node) = CCall (remapExpr version expr1) (remapExpr version <$> exprs) node
remapExpr version (CMember struct id deref node) = CMember (remapExpr version struct) (remapIdent version id) deref node
remapExpr version (CConst const ) = CConst const 
remapExpr version (CCompoundLit decl initList node) = CCompoundLit (remapDecl version decl) (remapInitList version initList) node
remapExpr version (CGenericSelection expr xs node) = 
    let f (mDecl, expr) = (remapDecl version <$> mDecl, remapExpr version expr)
        xs' = map f xs
        expr' = remapExpr version expr
    in CGenericSelection expr' xs' node
remapExpr version (CStatExpr stmt node) = CStatExpr (remapStmt version stmt) node
remapExpr version (CLabAddrExpr id node) = CLabAddrExpr (remapIdent version id) node
remapExpr version expr = expr

