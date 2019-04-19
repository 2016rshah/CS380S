module Main where

import Prelude hiding (log)

import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.InputStream
import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Analysis.AstAnalysis
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
import Control.Monad

-- import qualified Data.Map.Lazy as Map

type VarCount = (Int, Int)
type Log = [String]

data InstrumentationState = IState 
    { 
        notes :: Log, 
        source :: Maybe Ident,
        transformedFns :: [(FunDef, FunDef)],
        ast :: CTranslUnit
    }

instance Show InstrumentationState where
    show is = "Log: " ++ show (notes is) ++ "\n\n" ++
                "Source: " ++ showSource (source is) ++ "\n\n" ++
                "Transformed Functions: \n" ++ showFns (transformedFns is)
        where showSource (Just x) = show $ pretty x
              showSource Nothing = "No source specified"
              showFns = concatMap (\(f, f') -> "\t" ++ (show . pretty) f ++ " -> " ++ (show . pretty) f' ++ "\n")

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

emptyInstState :: String -> InstrumentationState
emptyInstState fileName = IState { 
                                    notes = [], 
                                    ast = CTranslUnit [] (OnlyPos pos (pos, 0)),
                                    transformedFns = [],
                                    source = Nothing
                                    }
    where pos = initialPos fileName 

main :: IO ()
main = do
        fp <- head <$> getArgs
        parseResult <- parseCFile (newGCC "gcc") Nothing [] fp
        let f (Left pr) = error $ show pr
            f (Right pr) = pr
            parsed@(CTranslUnit ed ni) = f parseResult
            traversal = buildCountingTraversal parsed
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
        -- printFormattedFilename fp 18
        -- print $ pretty parsed
        -- printFormattedFilename (fp ++ " Transformed") 12
        -- print $ pretty newAst
        -- print (pretty parsed == pretty newAst)
    where
        prettyAst ast@(CTranslUnit ed ni) = ppShow ed
        printFormattedFilename fp n = do
            putStrLn $ "|" ++ replicate (2 * n + length fp) '=' ++ "|"
            putStrLn $ "|" ++ replicate n ' ' ++ fp ++ replicate n ' ' ++ "|"
            putStrLn $ "|" ++ replicate (2 * n + length fp) '=' ++ "|"


instrumentationTraversal :: CTranslUnit -> Trav InstrumentationState CTranslUnit
instrumentationTraversal ast = 
        export <$> withExtDeclHandler innerTrav instrumentationDeclHandler
        -- (lg, ast') <- getUserState
        -- return ast'
    where innerTrav = analyseAST ast

instrumentFunction :: FunDef -> Trav InstrumentationState FunDef
instrumentFunction f@(FunDef var_decl body node_info) =
    do
    newBody <- instrumentFunctionBody node_info var_decl body
    return $ FunDef var_decl newBody node_info

instrumentFunctionBody :: NodeInfo -> VarDecl -> CStat -> Trav InstrumentationState CStat
instrumentFunctionBody node_info decl s@(CCompound localLabels items node_info_body) =
    do 
    enterFunctionScope
    mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels s)
    defineParams node_info decl
    -- record parameters
    items' <- mapM (instrumentBlockItem [FunCtx decl]) items
    log $ map (show . pretty) items
    log $ map (show . pretty) items'
    leaveFunctionScope
    return $ CCompound localLabels items' node_info_body
instrumentFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

instrumentBlockItem :: [StmtCtx] -> CBlockItem -> Trav InstrumentationState CBlockItem
instrumentBlockItem c (CBlockDecl d) = CBlockDecl <$> instrumentDecl d
instrumentBlockItem c b = return b

setStrConst :: String -> CDecl -> CDecl
setStrConst strConst (CDecl [typeSpecifier] [(Just declr, _, expr)] node_info) =
    -- TODO: node info needs fixing? I.E. not just inherited
    let initlr = CInitExpr (CConst (CStrConst (cString strConst) node_info)) node_info 
        typeSpecifier' = CTypeSpec (CCharType node_info)
        CDeclr id _ _ _ node_info2 = declr
        declr' = CDeclr id [CPtrDeclr [] node_info2] Nothing [] node_info2
    in CDecl [typeSpecifier] [(Just declr', Just initlr, expr)] node_info
setStrConst _ _ = error "Tried to set str const to invalid declaration"

-- setStrConst [(Just x, Just (CInitExpr (CConst (CStrConst _ ni3)) ni4), e)] =
--     [(Just x, Just (CInitExpr (CConst (CStrConst (cString "F") ni3)) ni4), e)]

instrumentDecl :: CDecl -> Trav InstrumentationState CDecl
instrumentDecl s@CStaticAssert{} = return s
instrumentDecl d@(CDecl typeSpecifier declr node_info) 
    | isSource d = return $ setStrConst "TEST" d
    | _ = return d

sources = ["foo", "bar"]

isSource :: CDecl -> Bool
isSource (CDecl typeSpecifier declr node_info) =
        case varName of
            Just name -> name `elem` sources
            Nothing   -> False
    where varName =
            case declr of
                [(Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _)] -> Just name
                _ -> Nothing
isSource _ = False


 -- copied
-- | Typecheck a block item. When statement expressions are blocks,
--   they have the type of their last expression statement, so this
--   needs to return a type.
tBlockItem :: MonadTrav m => [StmtCtx] -> CBlockItem -> m Type
tBlockItem c (CBlockStmt s) = tStmt c s
tBlockItem _ (CBlockDecl d) = analyseDecl True d >> return voidType
-- TODO: fixup analyseFunDef to handle nested functions
tBlockItem _ (CNestedFunDef fd) = analyseFunDef fd >> return voidType

instrumentedFunctions = ["main"]

shouldInstrumentFunction :: FunDef -> Trav s Bool
shouldInstrumentFunction f@(FunDef decl_info body node_info) = 
    case decl_info of
        (VarDecl (VarName (Ident name i n_i) asmName) x y) -> if name `elem` instrumentedFunctions
                                                                then return True
                                                                else return False
        _                                                  -> return False

instrumentationDeclHandler :: DeclEvent -> Trav InstrumentationState ()
instrumentationDeclHandler (DeclEvent (FunctionDef f)) = do
    b <- shouldInstrumentFunction f
    when b $ do
        f' <- instrumentFunction f
        addTransformedFn (f,f')
-- instrumentationDeclHandler (LocalEvent (ObjectDef o)) = do
--                                                             case getObjIdent o of
--                                                                 Nothing -> return ()
--                                                                 Just id@(Ident varName _ _) -> when (varName == "bar")
--                                                                                                     $ modifyUserState (\is -> is { source = Just id })
--                                                             dTable <- getDefTable
--                                                             st <- getUserState
--                                                             lookup <- case source st of
--                                                                     Just id -> lookupObject id
--                                                                     Nothing -> return Nothing
--                                                             return ()
instrumentationDeclHandler _                          = return ()
                                                            -- case lookup of
                                                            --     Just id -> modifyUserState $ log True
                                                            --     Nothing -> modifyUserState $ log False
                                                            -- modifyUserState $ log o
-- instrumentationDeclHandler (LocalEvent i) = modifyUserState $ log i
-- instrumentationDeclHandler (DeclEvent i)             = modifyUserState $ log i
-- instrumentationDeclHandler (TagEvent i)             = modifyUserState $ log i

getObjIdent :: ObjDef -> Maybe Ident
getObjIdent (ObjDef (VarDecl (VarName id _) _ _) _ _) = Just id
getObjIdent _ = Nothing

addToCount x y vc@(global, local) = (global+x, local+y)

countingTraversal :: Trav VarCount a -> Trav VarCount a
countingTraversal tr = do
    let f (DeclEvent (ObjectDef _)) = modifyUserState $ addToCount 1 0
        f (LocalEvent (ObjectDef o)) = modifyUserState $ addToCount 1 0
        f _ = modifyUserState id
    withExtDeclHandler tr f



-- editSources :: CTranslUnit -> Trav Log CTranslUnit
-- editSources (CTranslUnit decls _file_node) = do
--     -- analyse all declarations, but recover from errors
--     decls' <- mapM analyzeSources decls
--     return $ CTranslUnit decls' _file_node

-- analyzeSources :: CExtDecl -> Trav Log CExtDecl
-- analyzeSources d@(CDeclExt decl) = case decl of
--                                     d'@(CDecl s as ni) -> return $ CDeclExt $ CDecl s (setConst as) ni
--                                     _ -> return d
-- analyzeSources d@(CFDefExt f) = case getFunctionName f of
--                                 Just name -> modify $ \ts -> ts { userState = f (userState ts) }
--                                 modifyUserState $ addToLog name
--                                 Nothing -> return d
-- analyzeSources d = return d


getFunctionName :: CFunDef -> Maybe String
getFunctionName (CFunDef _ (CDeclr mId _ _ _ _) _ _ _) = mId >>= f 
    where f (Ident name _ _) = return name


analyzeInitialization :: ObjDef -> Bool
analyzeInitialization (ObjDef _ Nothing _) = False
analyzeInitialization (ObjDef _ (Just init) _) = case init of
                                            CInitExpr x _ -> True
                                            CInitList y _ -> False

buildCountingTraversal :: CTranslUnit -> Trav VarCount GlobalDecls
buildCountingTraversal ast = analyseAST ast 