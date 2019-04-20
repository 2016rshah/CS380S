module Main where

import Prelude hiding (log)

import Language.C
import Language.C.Data.Ident
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

data EntropicDependency =
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

log :: (Show a) => a -> Trav InstrumentationState ()
log = modifyUserState . addToLog

logPretty :: (Pretty a) => a -> Trav InstrumentationState ()
logPretty = modifyUserState . addPrettyToLog

addTransformedFn :: (FunDef, FunDef) -> Trav InstrumentationState ()
addTransformedFn f = modifyUserState $ \is -> is { transformedFns = transformedFns is ++ [f] }

addToSources :: Ident -> Trav InstrumentationState ()
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
            g (Left err) = error "Traversal error"
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


instrumentationTraversal :: CTranslUnit -> Trav InstrumentationState CTranslUnit
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

instrumentExt :: CExtDecl -> Trav InstrumentationState ()
instrumentExt (CAsmExt asm _) = handleAsmBlock asm
instrumentExt (CFDefExt fundef) = (if shouldInstrumentFunction fundef then instrumentFunction else analyseFunDef) fundef
instrumentExt (CDeclExt decl) = analyseDecl False decl

instrumentedFunctions = ["main"]

shouldInstrumentFunction :: CFunDef -> Bool
shouldInstrumentFunction (CFunDef declspecs declr oldstyle_decls stmt node_info) = 
    case declr of
        (CDeclr (Just (Ident name _ _)) _ _ _ _) -> name `elem` instrumentedFunctions
        _                                        -> False

instrumentFunction :: CFunDef -> Trav InstrumentationState ()
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


instrumentFunctionBody :: NodeInfo -> VarDecl -> CStat -> Trav InstrumentationState Stmt
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

instrumentBlockItem :: [StmtCtx] -> CBlockItem -> Trav InstrumentationState CBlockItem
instrumentBlockItem _ (CBlockDecl d) = CBlockDecl <$> instrumentDecl True d 
instrumentBlockItem _ (CNestedFunDef fundef) = 
    CNestedFunDef <$> do
        if shouldInstrumentFunction fundef 
        then instrumentFunction fundef
        else analyseFunDef fundef
        return fundef
instrumentBlockItem c (CBlockStmt s) = CBlockStmt <$> instrumentStmt c s 

instrumentStmt :: [StmtCtx] -> CStat -> Trav InstrumentationState CStat
instrumentStmt c = return 

setStrConst :: String -> CDecl -> CDecl
setStrConst strConst (CDecl [typeSpecifier] [(Just declr, _, expr)] node_info) =
    -- TODO: node info needs fixing? I.E. not just inherited
    let initlr = CInitExpr (CConst (CStrConst (cString strConst) node_info)) node_info 
        typeSpecifier' = CTypeSpec (CCharType node_info)
        CDeclr id _ _ _ node_info2 = declr
        declr' = CDeclr id [CPtrDeclr [] node_info2] Nothing [] node_info2
    in CDecl [typeSpecifier'] [(Just declr', Just initlr, expr)] node_info
setStrConst _ _ = error "Tried to set str const to invalid declaration"

instrumentDecl :: Bool -> CDecl -> Trav InstrumentationState CDecl
instrumentDecl _is_local s@CStaticAssert{} = return s
instrumentDecl is_local decl@(CDecl declspecs declrs node)
    | null declrs = return decl
    | otherwise   = do
        decl' <- instrumentDecl' decl
        analyseDecl is_local decl'
        return decl'

    where
    instrumentDecl' :: CDecl -> Trav InstrumentationState CDecl
    instrumentDecl' decl@(CDecl declspecs [(Just declr, _init, _)] node) =
        if isSource decl
        then 
            case identOfDecl decl of
                Nothing -> return decl
                Just id -> do
                    addToSources id
                    return $ setStrConst "SOURCE" decl
        else do
            dependency <- dependencyOnSource decl
            case dependency of
                Preserving -> return $ setStrConst "PRESERVING" decl
                Nonpreserving -> return $ setStrConst "NON-PRESERVING" decl
                _ -> return decl
    instrumentDecl' decl@CDecl{} = return decl
    instrumentDecl' CStaticAssert{} = error "Tried to instrument a static assertion"

annotatedSources = ["foo", "bar"]

isSource :: CDecl -> Bool
isSource d@(CDecl typeSpecifier declr node_info) =
        case identOfDecl d of 
            Just (Ident name _ _) -> name `elem` annotatedSources
            Nothing               -> False
isSource _ = False

dependencyOnSource :: CDecl -> Trav InstrumentationState EntropicDependency
dependencyOnSource (CDecl [typeSpecifier] 
                   [(Just declr, Just (CInitExpr expr node_info), size)] -- TODO: support for InitializerLists?
                   node_info2) = do
                        dTable <- getDefTable
                        st <- getUserState
                        let currentSources = sources st
                            lookupAll = mapMaybe ((`lookupIdent` dTable)) currentSources
                        if null lookupAll 
                        then return NoDependency 
                        else return Preserving
dependencyOnSource _ = return NoDependency
    
identOfDecl :: CDecl -> Maybe Ident
identOfDecl (CDecl _ declr _) = case declr of
                                    [(Just (CDeclr (Just id) _ _ _ _), _, _)] -> Just id
                                    _ -> Nothing