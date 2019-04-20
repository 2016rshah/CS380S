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
import Language.C.Analysis.DefTable hiding (enterFunctionScope, leaveFunctionScope)
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
import Control.Monad

type VarCount = (Int, Int)
type Log = [String]
type InstTrav = Trav InstrumentationState  

data EntropicDependency =
    Source |
    Preserving |
    Nonpreserving |
    NoDependency

data InstrumentationState = IState 
    { 
        notes :: Log, 
        sources :: [Ident],
        transformedFns :: [(FunDef, FunDef)],
        ast :: CTranslUnit
    }

instance Show InstrumentationState where
    show is = "Log: " ++ show (notes is) ++ "\n\n" ++
                "Sources: " ++ show (map (show . pretty) (sources is)) ++ "\n\n" ++
                "Transformed Functions: \n" ++ showFns (transformedFns is)
        where showFns = concatMap (\(f, f') -> "\t" ++ (show . pretty) f ++ " -> " ++ (show . pretty) f' ++ "\n")

instance Pretty Bool where
    pretty = text . show 

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

addTransformedFn :: (FunDef, FunDef) -> InstTrav ()
addTransformedFn f = modifyUserState $ \is -> is { transformedFns = transformedFns is ++ [f] }

addToSources :: Ident -> InstTrav ()
addToSources s = modifyUserState $ \is -> is { sources = sources is ++ [s] }

emptyInstState :: String -> InstrumentationState
emptyInstState fileName = IState { 
                                    notes = [], 
                                    ast = CTranslUnit [] (OnlyPos pos (pos, 0)),
                                    transformedFns = [],
                                    sources = []
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
    blocks' <- mapM (instrumentBlockItem []) blocks
    return $ CCompound localLabels blocks' node
instrumentStmt c (CWhile guard body isDoWhile node) = do
    guard' <- instrumentExpr c RValue guard
    body' <- instrumentStmt c body
    return $ CWhile guard' body' isDoWhile node
instrumentStmt c (CIf ifExpr thenStmt maybeElse node) = do
    ifExpr' <- instrumentExpr c RValue ifExpr
    thenStmt' <- instrumentStmt c thenStmt
    maybeElse' <- maybe (return Nothing) (\x -> Just <$> instrumentStmt c x) maybeElse
    return $ CIf ifExpr' thenStmt' maybeElse' node
instrumentStmt c (CFor init expr2 expr3 stmt node) = do
    let mkExpr  e = Left <$> mkMaybeExpr c e
        mkDeclr d = Right <$> instrumentDecl True d
    init' <- either mkExpr mkDeclr init
    expr2' <- mkMaybeExpr c expr2
    expr3' <- mkMaybeExpr c expr3
    stmt' <- instrumentStmt c stmt
    return $ CFor init' expr2' expr3' stmt' node
instrumentStmt c (CReturn expr node) = do
    expr' <- mkMaybeExpr c expr
    return $ CReturn expr' node
instrumentStmt _ CSwitch{} = error "Case statements not supported yet"
instrumentStmt _ CCase{} = error "Case statements not supported yet"
instrumentStmt _ CCases{} = error "Case statements not supported yet"
instrumentStmt _ CDefault{} = error "Case statements not supported yet"
instrumentStmt _ s = return s

mkMaybeExpr c = maybe (return Nothing) (\x -> Just <$> instrumentExpr c RValue x)

instrumentExpr :: [StmtCtx] -> ExprSide -> CExpr -> InstTrav CExpr
instrumentExpr c _ expr@(CAssign op lhs rhs node) = do
        let lhs' = setIndirections lhs
        rhs' <- processRhs rhs
        return $ CAssign op lhs' rhs' node
    where
    setIndirections var@(CVar id node) = CUnary CIndOp var node
    setIndirections (CUnary CIndOp expr node) = setIndirections expr
    setIndirections _ = error "Setting indirections failed"
instrumentExpr c _ expr@(CConst (CStrConst s node)) = if getCString s == "preservational" 
                                                 then return $ makeStrConst "PRESERVING_RETURN" expr
                                                 else return expr
instrumentExpr c side expr = return expr

processRhs :: CExpr -> InstTrav CExpr
processRhs rhs = do
    dependency <- exprDependency rhs
    case dependency of
        Preserving -> return $ makeStrConst "PRESERVING" rhs
        Nonpreserving -> return $ makeStrConst "NON-PRESERVING" rhs
        _ -> return rhs

makeStrConst :: String -> CExpr -> CExpr
makeStrConst strConst expr = let node_info = nodeInfo expr 
                             in CConst $ CStrConst (cString strConst) node_info

setStrConst :: String -> CDecl -> CDecl
setStrConst strConst (CDecl [typeSpecifier] [(Just declr, _, expr)] node_info) =
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
            Preserving -> return $ setStrConst "PRESERVING" decl
            Nonpreserving -> return $ setStrConst "NON-PRESERVING" decl
            Source -> case identOfDecl decl of
                        Nothing -> return decl
                        Just id -> do
                            addToSources id
                            return $ setStrConst "SOURCE" decl
            _ -> return decl
    instrumentDecl' decl@CDecl{} = return decl
    instrumentDecl' CStaticAssert{} = error "Tried to instrument a static assertion"

annotatedSources = ["foo", "bar"]

exprDependency :: CExpr -> InstTrav EntropicDependency
exprDependency expr = do
        dTable <- getDefTable
        st <- getUserState
        let currentSources = sources st
            lookupAll = mapMaybe (`lookupIdent` dTable) currentSources
        if null lookupAll 
        then return NoDependency 
        else return Preserving

declDependency :: CDecl -> InstTrav EntropicDependency
declDependency decl@(CDecl [typeSpecifier] 
                     [(Just declr, Just (CInitExpr expr node_info), size)] -- TODO: support for InitializerLists?
                     node_info2) =
                        if isSource decl
                        then return Source
                        else do
                            dTable <- getDefTable
                            st <- getUserState
                            let currentSources = sources st
                                lookupAll = mapMaybe (`lookupIdent` dTable) currentSources
                            if null lookupAll 
                            then return NoDependency 
                            else return Preserving
    where
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