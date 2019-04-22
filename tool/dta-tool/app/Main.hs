module Main where

import Prelude hiding (log)

import Language.C
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.InputStream
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.SemError
import Language.C.Analysis.DeclAnalysis
import Language.C.Analysis.TypeUtils
import Language.C.Syntax.Utils
import Language.C.Analysis.DefTable hiding (enterFunctionScope, leaveFunctionScope,
                                            enterBlockScope, leaveBlockScope)
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Analysis.Export
import Language.C.System.GCC
import Language.C.Parser
import Language.C.Pretty
import System.IO
import System.Environment

import Text.Show.Pretty
import Text.PrettyPrint

import Data.List
import Data.Maybe 
import qualified Data.Map as Map
import Control.Monad

type VarCount = (Int, Int)
type Log = [String]
type InstTrav = Trav InstrumentationState 
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

data FunInstState = FIState 
    {
        ctx :: VarDecl, 
        ogFnBody :: [CBlockItem], 
        newFnBody ::[CBlockItem]
    }

data InstrumentationState = IState 
    { 
        notes :: Log, 
        taints :: Taints,
        functionContext :: FunInstState,
        ast :: CTranslUnit
    }

instance Show InstrumentationState where
    show is = "Log: " ++ show (notes is) ++ "\n\n" ++
                "Taint values: " ++ show (Map.mapKeys (show . pretty) (taints is)) ++ "\n\n" 

instance Pretty Bool where
    pretty = text . show

initFunInstStateWithContext c = FIState { ctx = c, ogFnBody = [], newFnBody = [] }
initFunInstState = initFunInstStateWithContext $ VarDecl NoName (DeclAttrs noFunctionAttrs NoStorage noAttributes) voidType 

resetFunContext c = modifyUserState $ \st -> st { functionContext = initFunInstStateWithContext c }
reinitFunContext = resetFunContext $ VarDecl NoName (DeclAttrs noFunctionAttrs NoStorage noAttributes) voidType

functionInstrumentationSetup :: VarDecl -> InstTrav ()
functionInstrumentationSetup c = resetFunContext c >> enterFunctionScope

functionInstrumentationFinalize :: InstTrav [CBlockItem]
functionInstrumentationFinalize = do
    leaveFunctionScope
    body <- newFnBody . functionContext <$> getUserState
    reinitFunContext
    return body

addNewBlockItem :: CBlockItem -> InstTrav ()
addNewBlockItem item = do
    ctxt <- functionContext <$> getUserState
    let body = newFnBody ctxt
    modifyUserState $ \st -> st { functionContext = ctxt { newFnBody = body ++ [item] } }

initialPos :: String -> Position
initialPos fileName = position 0 fileName 1 0 Nothing

addPrettyToLog :: (Pretty a) => a -> InstrumentationState -> InstrumentationState
addPrettyToLog o is = let lg = notes is ++ [show (pretty o)] in is { notes = lg }

addToLog :: (Show a) => a -> InstrumentationState -> InstrumentationState
addToLog o is = let lg = notes is ++ [show o] in is { notes = lg }

log :: (Show a) => a -> InstTrav ()
log = modifyUserState . addToLog

logPretty :: (Pretty a) => a -> InstTrav ()
logPretty = modifyUserState . addPrettyToLog

addToTaints :: Ident -> EntropicDependency -> InstTrav ()
addToTaints id t = modifyUserState $ \is -> is { taints = Map.insert id t (taints is) }

emptyInstState :: String -> InstrumentationState
emptyInstState fileName = IState { 
                                    notes = [], 
                                    ast = CTranslUnit [] (OnlyPos pos (pos, 0)),
                                    functionContext = initFunInstState,
                                    taints = Map.empty
                                    }
    where pos = initialPos fileName 

main :: IO ()
main = do
        fp <- head <$> getArgs
        parseResult <- parseCFile (newGCC "gcc") Nothing [] fp
        let f (Left pr) = error $ show pr
            f (Right pr) = pr
            parsed@(CTranslUnit ed ni) = f parseResult
            g (Left err) = error $ "Traversal error: " ++ show err
            g (Right (result, state)) = (result, userState state)
            (traversalResult, traversalState) = g $ runTrav (emptyInstState fp) $ instrumentationTraversal parsed
        print traversalState
        let astOutput = prettyAst parsed
        writeFile "astEdited.hs" astOutput
        let newAst = case parseC (inputStreamFromString $ show $ pretty traversalResult) (initialPos fp) of
                        (Left parseError) -> error "Parse error"
                        (Right newTranslUnit) -> newTranslUnit
        writeFile "globalDecls.hs" $ prettyAst newAst
        printFormattedFilename fp 18
        print $ pretty parsed
        printFormattedFilename (fp ++ " Transformed") 12
        print $ pretty newAst
    where
        prettyAst ast@(CTranslUnit ed ni) = ppShow ed
        printFormattedFilename fp n = do
            putStrLn $ "|" ++ replicate (2 * n + length fp) '=' ++ "|"
            putStrLn $ "|" ++ replicate n ' ' ++ fp ++ replicate n ' ' ++ "|"
            putStrLn $ "|" ++ replicate (2 * n + length fp) '=' ++ "|"


instrumentationTraversal :: CTranslUnit -> InstTrav CTranslUnit
instrumentationTraversal (CTranslUnit decls _file_node) = do
    -- instrument all declarations, but recover from errors
    mapRecoverM_ instrumentExt decls
    -- check we are in global scope afterwards
    dTable <- getDefTable
    getDefTable >>= (\dt -> unless (inFileScope dt) $ error "Internal Error: Not in filescope after analysis")
    -- get the global definition table and export to an AST
    export . globalDefs <$> getDefTable
    where
    mapRecoverM_ f = mapM_ (handleTravError . f)

instrumentExt :: CExtDecl -> InstTrav ()
instrumentExt (CAsmExt asm _) = handleAsmBlock asm
instrumentExt (CFDefExt fundef) = if shouldInstrumentFunction fundef 
                                  then void $ instrumentFunction fundef 
                                  else analyseFunDef fundef
instrumentExt (CDeclExt decl) = analyseDecl False decl

instrumentedFunctions = ["main", "g"]

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


instrumentFunctionBody :: NodeInfo -> VarDecl -> CStat -> InstTrav Stmt
instrumentFunctionBody node_info decl s@(CCompound localLabels items node_info_body) =
    do 
        functionInstrumentationSetup decl
        mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels s)
        defineParams node_info decl
        -- record parameters
        mapM_ (newBlockItem [FunCtx decl]) items
        let id = declIdent decl
        items' <- functionInstrumentationFinalize
        return $ CCompound localLabels items' node_info_body
instrumentFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

newBlockItem :: [StmtCtx] -> CBlockItem -> InstTrav ()
newBlockItem c item = addNewBlockItem =<< instrumentBlockItem c item

instrumentBlockItem :: [StmtCtx] -> CBlockItem -> InstTrav CBlockItem
instrumentBlockItem _ (CBlockDecl d) = CBlockDecl <$> instrumentDecl True d 
instrumentBlockItem _ (CNestedFunDef fundef) = 
    CNestedFunDef <$> 
        if shouldInstrumentFunction fundef 
        then instrumentFunction fundef
        else analyseFunDef fundef >> return fundef
instrumentBlockItem c (CBlockStmt s) = CBlockStmt <$> instrumentStmt c s 

instrumentStmt :: [StmtCtx] -> CStat -> InstTrav CStat
instrumentStmt c s@(CExpr expr node) =
    case expr of
        Nothing -> return s
        Just e -> do
            expr' <- instrumentExpr c RValue e
            return $ CExpr (Just expr') node
instrumentStmt c (CCompound localLabels blocks node) = do
    enterBlockScope
    blocks' <- mapM (instrumentBlockItem []) blocks
    leaveBlockScope
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
                        setTaintMap $ Map.unionWith min thenTaintMap elseTaintMap
                        return $ Just newElse
    return $ CIf ifExpr' thenStmt' maybeElse' node
instrumentStmt c (CFor init expr2 expr3 stmt node) = do
    let mkExpr  e = Left <$> mkMaybeExpr c e
        mkDeclr d = Right <$> instrumentDecl True d
    enterBlockScope
    init' <- either mkExpr mkDeclr init
    expr2' <- mkMaybeExpr c expr2
    expr3' <- mkMaybeExpr c expr3
    stmt' <- instrumentStmt c stmt
    leaveBlockScope
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

instrumentExpr :: [StmtCtx] -> ExprSide -> CExpr -> InstTrav CExpr
instrumentExpr c side expr@(CAssign op lhs rhs node) = do
        dependency <- assignmentOpDependency op <$> exprDependency c side rhs
        let idLhs = fromJust $ identOfExpr lhs
        addToTaints idLhs dependency
        case dependency of
            HighEntropy -> do
                let rhs' = processHighEntropyRhs rhs
                    lhs' = setIndirections lhs
                -- dt <- getDefTable
                -- -- let t = maybe Nothing (lookupType dt) (nameOfNode (nodeInfo rhs))
                -- -- t <- tExpr c side lhs
                -- t <- tExpr c side expr
                -- log $ show $ pretty t
                return $ CAssign CAssignOp lhs' rhs' node
            LowEntropy -> do
                let rhs' = processLowEntropyRhs rhs
                    lhs' = setIndirections lhs
                return $ CAssign CAssignOp lhs' rhs' node
            _ -> return expr
    where
        setIndirections var@(CVar id node) = CUnary CIndOp var node
        setIndirections (CIndex arr _ _) = setIndirections arr
        setIndirections (CUnary CIndOp expr node) = setIndirections expr
        setIndirections _ = error "Setting indirections failed"

        processHighEntropyRhs rhs@CCond{} = rhs
        processHighEntropyRhs rhs = makeStrConst "HIGH ENTROPY" rhs

        processLowEntropyRhs rhs@CCond{} = rhs
        processLowEntropyRhs rhs = makeStrConst "LOW ENTROPY" rhs
instrumentExpr c side (CComma exprs node) = do
        exprs' <- mapM (instrumentExpr c side) exprs
        return $ CComma exprs' node 
instrumentExpr c side expr = return expr

setNodeType :: (MonadTrav m) => NodeInfo -> Type -> m ()
setNodeType node t = 
    case nameOfNode node of
        Just n -> withDefTable (\dt -> ((), insertType dt n t))
        Nothing -> return ()

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
    instrumentDecl' decl@(CDecl declspecs [(Just declr, _init, _)] node) = do
        dependency <- declDependency decl
        case dependency of
            HighEntropy -> return $ setStrConst "HIGH ENTROPY" decl
            LowEntropy -> return $ setStrConst "LOW ENTROPY" decl
            Source -> case identOfDecl decl of
                        Nothing -> return decl
                        Just id -> do
                            addToTaints id Source
                            return $ setStrConst "SOURCE" decl
            _ -> return decl
    instrumentDecl' decl@CDecl{} = return decl
    instrumentDecl' CStaticAssert{} = error "Tried to instrument a static assertion"

annotatedSources = ["foo", "baz"]--["foo", "bar"]

getTaintValue :: Ident -> InstTrav EntropicDependency
getTaintValue id = do
    st <- getUserState
    return $ Map.findWithDefault NoDependency id $ taints st

getTaintMap :: InstTrav Taints
getTaintMap = taints <$> getUserState

setTaintMap :: Taints -> InstTrav ()
setTaintMap tm = modifyUserState (\st -> st { taints = tm })

exprDependency :: [StmtCtx] -> ExprSide -> CExpr -> InstTrav EntropicDependency
exprDependency _ LValue _ = return NoDependency
exprDependency c side (CCast declr expr _node) = exprDependency c side expr
exprDependency c side (CUnary op expr _node) = unaryOpDependency op <$> exprDependency c side expr
exprDependency c side (CBinary op expr1 expr2 _node) = 
    liftM2 (binaryOpDependency op) (exprDependency c side expr1) (exprDependency c side expr2)
exprDependency c side (CConst _) = return NoDependency
exprDependency c side (CComplexReal expr _node) = applyOpDep Preserving <$> exprDependency c side expr
exprDependency c side (CComplexImag expr _node) = applyOpDep Nonpreserving <$> exprDependency c side expr
exprDependency c side (CCall fn args _) = 
    let fnName = maybe "" identToString (identOfExpr fn)
    in functionCallDependency fnName <$> mapM (exprDependency c side) args
exprDependency c _ (CVar id node) = getTaintValue id
exprDependency c side (CSizeofExpr expr _) = applyOpDep Nonpreserving <$> exprDependency c side expr
exprDependency c side (CSizeofType declr _) = applyOpDep Nonpreserving <$> declDependency declr
exprDependency c side (CAlignofExpr expr _) = applyOpDep Nonpreserving <$> exprDependency c side expr
exprDependency c side (CAlignofType declr _) = applyOpDep Nonpreserving <$> declDependency declr
exprDependency c side (CCond cond maybeTrueExpr falseExpr _node) = 
    let falseExprDep = exprDependency c side falseExpr 
        trueExprDep = case maybeTrueExpr of
                        Just trueExpr -> exprDependency c side trueExpr
                        Nothing -> return NoDependency
    in liftM2 min falseExprDep trueExprDep
exprDependency _ _ _ = return NoDependency

declDependency :: CDecl -> InstTrav EntropicDependency
declDependency decl@(CDecl [typeSpecifier] 
                     [(Just declr, expr, size)] -- TODO: support for InitializerLists?
                     node_info2) =
                        case expr of
                            Just (CInitExpr expr node_info) -> calcDependency decl
                            Nothing                         -> calcDependency decl
                            _                               -> return NoDependency
                        
    where
        calcDependency decl = 
            if isSource decl
            then return Source
            else getTaintValue $ fromJust $ identOfDecl decl
        isSource :: CDecl -> Bool
        isSource d@(CDecl typeSpecifier declr node_info) =
                case identOfDecl d of 
                    Just (Ident name _ _) -> name `elem` annotatedSources
                    Nothing               -> False
        isSource _ = False
declDependency _ = return NoDependency
    
identOfDecl :: CDecl -> Maybe Ident
identOfDecl (CDecl _ declr _) = case declr of
                                    [(Just (CDeclr (Just id) _ _ _ _), _, _)] -> Just id
                                    _ -> Nothing

identOfExpr :: CExpr -> Maybe Ident
identOfExpr (CVar id _) = Just id
identOfExpr (CUnary CIndOp expr _) = identOfExpr expr
identOfExpr (CIndex arr _ _) = identOfExpr arr
identOfExpr _ = Nothing


-- dTable <- getDefTable
-- st <- getUserState
-- let currentSources = Map.keys $ taints st
--     lookupAll = mapMaybe (`lookupIdent` dTable) currentSources
-- if null lookupAll 
-- then return NoDependency 
-- else return HighEntropy