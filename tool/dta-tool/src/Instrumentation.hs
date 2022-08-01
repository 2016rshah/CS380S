module Instrumentation(
instrumentation
)
where
import Prelude hiding (log)

import Utils
import EntropicDependency
import DependencyFunction
import InstrumentationState (emptyInstState)
import InstrumentationTrav
import TaintMap
import TaintEnv

import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Syntax.Utils
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Analysis.Export (exportDeclr, export)
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.SemRep
import Language.C.Analysis.SemError
import Language.C.Analysis.TravMonad hiding (maybeM, enterFunctionScope, leaveFunctionScope, 
                                             enterBlockScope, leaveBlockScope)
import Language.C.Analysis.DefTable hiding (enterFunctionScope, leaveFunctionScope,
                                            enterBlockScope, leaveBlockScope)
import Language.C.Pretty

import qualified Data.Map as Map
import Data.Maybe 
import Control.Monad
import Control.Arrow

instrumentation :: CTranslUnit -> (CTranslUnit, TaintEnv)
instrumentation ast = runTravOrDie_ emptyInstState $ instrumentationTraversal ast

instrumentationTraversal :: CTranslUnit -> InstTrav (CTranslUnit, TaintEnv)
instrumentationTraversal (CTranslUnit decls _file_node) = do
    -- instrument all declarations, but recover from errors
    mapRecoverM_ instrumentExt decls
    -- check we are in global scope afterwards
    dTable <- getDefTable
    -- log $ show $ Map.keys $ gObjs $ globalDefs dTable
    getDefTable >>= (\dt -> unless (inFileScope dt) $ error "Internal Error: Not in filescope after analysis")
    -- get the global definition table and export to an AST
    ast <- export . globalDefs <$> getDefTable
    taintEnv <- getTaintEnv
    return (ast, taintEnv)
    where
    mapRecoverM_ f = mapM_ (handleTravError . f)

instrumentExt :: CExtDecl -> InstTrav ()
instrumentExt (CAsmExt asm _) = handleAsmBlock asm
instrumentExt (CFDefExt fundef) = if shouldInstrumentFunction fundef 
                                  then void $ instrumentFunction fundef 
                                  else analyseFunDef fundef
instrumentExt (CDeclExt decl) = analyseDecl False decl

instrumentedFunctions = ["main1", "g"]

shouldInstrumentFunction :: CFunDef -> Bool
shouldInstrumentFunction (CFunDef declspecs declr oldstyle_decls stmt node_info) = 
    case declr of
        (CDeclr (Just (Ident name _ _)) _ _ _ _) -> name `elem` instrumentedFunctions
        _                                        -> False

instrumentFunction :: CFunDef -> InstTrav CFunDef
instrumentFunction (CFunDef declspecs declr oldstyle_decls stmt node_info) = do
    -- analyse the declarator
    var_decl_info <- analyseVarDecl' True declspecs declr oldstyle_decls Nothing
    let (VarDeclInfo name fun_spec storage_spec attrs ty _declr_node) = var_decl_info
    when (isNoName name) $ astError node_info "NoName in analyseFunDef"
    let ident = identOfVarName name
    -- improve incomplete type
    ty' <- improveFunDefType ty
    -- compute storage
    fun_storage <- computeFunDefStorage ident storage_spec
    let var_decl = VarDecl name (DeclAttrs fun_spec fun_storage attrs) ty'
    -- callback for declaration
    handleVarDecl False (Decl var_decl node_info)
    -- instrument body
    stmt' <- instrumentFunctionBody node_info var_decl stmt
    -- callback for definition
    handleFunDef ident (FunDef var_decl stmt' node_info)
    return $ CFunDef declspecs declr oldstyle_decls stmt' node_info
    where
    improveFunDefType (FunctionType (FunTypeIncomplete return_ty) attrs) =
      return $ FunctionType (FunType return_ty [] False) attrs
    improveFunDefType ty = return ty
    -- computeFunDefStorage copied from Language.C.Analysis.AstAnalysis
    -- | compute storage of a function definition
    --
    -- a function definition has static storage with internal linkage if specified `static`,
    -- the previously declared linkage if any if 'extern' or no specifier are present. (See C99 6.2.2, clause 5)
    --
    -- This function won't raise an Trav error if the declaration is incompatible with the existing one,
    -- this case is handled in 'handleFunDef'.
    computeFunDefStorage :: (MonadTrav m) => Ident -> StorageSpec -> m Storage
    computeFunDefStorage _ (StaticSpec _)  = return$ FunLinkage InternalLinkage
    computeFunDefStorage ident other_spec  = do
        obj_opt <- lookupObject ident
        let defaultSpec = FunLinkage ExternalLinkage
        case other_spec of
            NoStorageSpec  -> return$ maybe defaultSpec declStorage obj_opt
            (ExternSpec False) -> return$ maybe defaultSpec declStorage obj_opt
            bad_spec -> throwTravError $ badSpecifierError (nodeInfo ident)
                        $ "unexpected function storage specifier (only static or extern is allowed)" ++ show bad_spec


instrumentFunctionBody :: NodeInfo -> VarDecl -> CStat -> InstTrav CStat
instrumentFunctionBody node_info decl s@(CCompound localLabels items node_info_body) =
    do 
        enterFunctionScope
        mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels s)
        defineParams node_info decl
        -- record parameters
        items' <- mapM (instrumentBlockItem [FunCtx decl]) items
        leaveFunctionScope
        return $ CCompound localLabels items' node_info_body
instrumentFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

instrumentBlockItem :: [StmtCtx] -> CBlockItem -> InstTrav CBlockItem
instrumentBlockItem _ (CBlockDecl d) = CBlockDecl <$> instrumentDecl True d
instrumentBlockItem _ (CNestedFunDef fundef) = do
    fundef <- if shouldInstrumentFunction fundef 
              then instrumentFunction fundef
              else analyseFunDef fundef >> return fundef
    return $ CNestedFunDef fundef
instrumentBlockItem c (CBlockStmt s) = CBlockStmt <$> instrumentStmt c s 

instrumentStmt :: [StmtCtx] -> CStat -> InstTrav CStat
instrumentStmt c s@(CExpr expr node) =
    case expr of
        Nothing -> return s
        Just e -> do
            expr' <- instrumentExpr c RValue e
            return $ CExpr (Just expr') node
instrumentStmt c (CCompound localLabels blocks node) = do
    blocks' <- mapM (instrumentBlockItem []) blocks
    return $ CCompound localLabels blocks' node
instrumentStmt c (CWhile guard body isDoWhile node) = do
    guard' <- instrumentExpr c RValue guard
    body' <- instrumentStmt c body
    return $ CWhile guard' body' isDoWhile node
instrumentStmt c (CIf ifExpr thenStmt maybeElse node) = do
    prevTaintMap <- getTaintMap
    ifExpr' <- instrumentExpr c RValue ifExpr
    thenStmt' <- instrumentStmt c thenStmt
    thenTaintMap <- getTaintMap
    maybeElse' <- case maybeElse of
                    Nothing -> return Nothing
                    Just elseExpr -> do
                        setTaintMap prevTaintMap
                        newElse <- instrumentStmt c elseExpr
                        elseTaintMap <- getTaintMap
                        let mergedTaintMap = mergeTaintMap combineDependenciesBranch thenTaintMap elseTaintMap
                        setTaintMap mergedTaintMap 
                        return $ Just newElse
    return $ CIf ifExpr' thenStmt' maybeElse' node
instrumentStmt c (CFor init expr2 expr3 stmt node) = do
    let mkExpr  e = Left <$> mkMaybeExpr c e
        mkDeclr d = Right <$> instrumentDecl True d
    init' <- either mkExpr mkDeclr init
    expr2' <- mkMaybeExpr c expr2
    expr3' <- mkMaybeExpr c expr3
    stmt' <- instrumentStmt c stmt
    return $ CFor init' expr2' expr3' stmt' node
instrumentStmt c (CSwitch selectorExpr switchStmt node) = do
    selectorExpr' <- instrumentExpr c RValue selectorExpr
    switchStmt' <- instrumentStmt c switchStmt
    return $ CSwitch selectorExpr' switchStmt' node
instrumentStmt c (CCases lower upper stmt node) = do
        lower' <- instrumentExpr c RValue lower
        upper' <- instrumentExpr c RValue upper
        stmt' <- instrumentStmt c stmt
        return $ CCases lower' upper' stmt' node
instrumentStmt c (CCase expr stmt node) = do
    expr' <- instrumentExpr c RValue expr
    stmt' <- instrumentStmt c stmt
    return $ CCase expr' stmt' node
instrumentStmt c (CDefault stmt node) = do
    stmt' <- instrumentStmt c stmt
    return $ CDefault stmt' node
instrumentStmt _ s = return s

mkMaybeExpr c = maybe (return Nothing) (\x -> Just <$> instrumentExpr c RValue x)

processRhs :: [StmtCtx] -> CAssignOp -> Type -> CExpr -> InstTrav (EntropicDependency, CExpr)
processRhs c op returnType (CCond cond maybeTrueExpr falseExpr _node) = do
    (trueDep, trueExpr') <- case maybeTrueExpr of
                                Just trueExpr -> (\(d,e) -> (d,Just e)) <$> processRhs c op returnType trueExpr
                                Nothing -> return (NoDependency, Nothing) 
    (falseDep, falseExpr') <- processRhs c op returnType falseExpr
    let dep = combineDependencies trueDep falseDep
        rhs' = CCond cond trueExpr' falseExpr' _node
    return (dep, rhs')
processRhs c op returnType rhs = do
    (dep, vars) <- exprDependency rhs
    varTypes <- mapM typeOfVar vars
    let varNames = map (identToString . fromJust . identOfExpr) vars
        dep' = assignmentOpDependency op dep
    case dep' of
        NoDependency -> return (dep', rhs)
        Source -> return (dep', rhs)
        HighEntropy -> do
            fn <- fnCall Preserving returnType vars varTypes
            return (dep', fn)
        LowEntropy -> do
            fn <- fnCall Nonpreserving returnType vars varTypes
            return (dep', fn)
    where
        typeOfVar = tExpr c RValue
        fnCall depType returnType args argTypes = do
            fnId <- getDependencyFunction depType returnType argTypes
            return $ CCall (CVar fnId undefNode) args undefNode

processAsgmt :: [StmtCtx] -> CAssignOp -> CExpr -> CExpr -> InstTrav CExpr
processAsgmt c op lhs rhs = do
    returnType <- tExpr c LValue lhs
    (dep, rhs') <- processRhs c op returnType rhs
    let idLhs = fromJust $ identOfExpr lhs
    updateTaint idLhs dep
    return rhs'

exprDependency :: CExpr -> InstTrav (EntropicDependency, [CExpr])
exprDependency (CCast declr expr _node) = exprDependency expr
exprDependency (CUnary op expr _node) = singleOpDependency expr $ unaryOpDependency op
exprDependency (CBinary op expr1 expr2 _node) = do
    (dep1, vars1) <- exprDependency expr1
    (dep2, vars2) <- exprDependency expr2
    let dep' = binaryOpDependency op dep1 dep2
    return (dep', vars1 ++ vars2)
exprDependency (CConst _) = return (NoDependency, [])
exprDependency (CComplexReal expr _node) = singleOpDependency expr $ applyOpDep Preserving
exprDependency (CComplexImag expr _node) = singleOpDependency expr $ applyOpDep Nonpreserving
exprDependency (CCall fn args _) = do
    (deps, varss) <- unzip <$> mapM exprDependency args
    let fnName = maybe "" identToString (identOfExpr fn)
        dep' = functionCallDependency fnName deps
        vars = concat varss
    return (dep', vars)
exprDependency var@(CVar id node) = do
    dep <- getTaintValue id
    return (dep, [var])
exprDependency (CSizeofExpr expr _) = singleOpDependency expr $ applyOpDep Nonpreserving
exprDependency (CAlignofExpr expr _) = singleOpDependency expr $ applyOpDep Nonpreserving
exprDependency (CSizeofType declr _) = exprDependency (CAlignofType declr undefNode)
exprDependency (CAlignofType declr _) = case identOfDecl declr of
                                            Nothing -> return (NoDependency, [])
                                            Just id -> (\t -> (t, [CVar id undefNode])) <$> getTaintValue id
exprDependency (CCond cond maybeTrueExpr falseExpr _node) = do
    (dep1, vars1) <- exprDependency falseExpr 
    (dep2, vars2) <- case maybeTrueExpr of
                    Just trueExpr -> exprDependency trueExpr
                    Nothing -> return (NoDependency, [])
    return (min dep1 dep2, vars1++vars2)
exprDependency  _ = return (NoDependency, [])

singleOpDependency :: CExpr -> (EntropicDependency -> EntropicDependency) -> InstTrav (EntropicDependency, [CExpr])
singleOpDependency expr depFn = do
    (dep, vars) <- exprDependency expr
    let dep' = depFn dep
    return (dep', vars)

instrumentExpr :: [StmtCtx] -> ExprSide -> CExpr -> InstTrav CExpr
instrumentExpr c side expr@(CAssign op lhs rhs node) = do
        rhs' <- processAsgmt c op lhs rhs
        return $ CAssign CAssignOp lhs rhs' node
instrumentExpr c side (CComma exprs node) = do
        exprs' <- mapM (instrumentExpr c side) exprs
        return $ CComma exprs' node 
instrumentExpr c side expr = return expr

instrumentDecl :: Bool -> CDecl -> InstTrav CDecl
instrumentDecl _is_local s@CStaticAssert{} = return s
instrumentDecl is_local decl@(CDecl declspecs declrs node)
    | null declrs = return decl
    | otherwise   = do
        decl' <- instrumentDecl' decl
        analyseDecl is_local decl'
        return decl'
    where
    instrumentDecl' :: CDecl -> InstTrav CDecl
    instrumentDecl' decl@(CDecl declspecs [(Just declr, init, _x)] node) = do
        let id = fromJust $ identOfDecl decl
        if isSource decl
        then do
            addToTaints id Source
            return $ setStrConst "SOURCE" decl
        else
            case init of
                Nothing                         -> addToTaints id NoDependency >> return decl
                Just (CInitExpr expr node_info) -> do
                    returnType <- tExpr [] RValue expr
                    (dep, expr') <- processRhs [] CAssignOp returnType expr
                    let init' = CInitExpr expr' node_info
                    addToTaints id dep
                    let decl' = CDecl declspecs [(Just declr, Just init', _x)] node
                    return decl'
    instrumentDecl' decl@CDecl{} = return decl
    instrumentDecl' CStaticAssert{} = error "Tried to instrument a static assertion"

annotatedSources = ["foo", "baz"]
-- the type of sources is char*
sourceType = PtrType (DirectType (TyIntegral TyChar) noTypeQuals noAttributes) noTypeQuals noAttributes

isSource :: CDecl -> Bool
isSource d@(CDecl typeSpecifier declr node_info) =
        case identOfDecl d of 
            Just (Ident name _ _) -> name `elem` annotatedSources
            Nothing               -> False
isSource _ = False