{-# LANGUAGE FlexibleInstances #-}

module VarRemap(
ProgramVersion(..),
VRState,
    remapping,
emptyVRState,
VRTrav,
remapProgram,
versionSuffix, getSuffix
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
type VarRemapping =  Map.Map Ident Ident

data VRState = VRState
    {
        notes :: String,
        version :: ProgramVersion,
        remapping :: VarRemapping
    }

type VRTrav = Trav VRState

instance Loggable VRTrav where
    log = modifyUserState . addToLog
    logPretty = modifyUserState . addPrettyToLog

instance Show VRState where
    show st = "Log: " ++ notes st ++ "\n\n" ++
                "Variable Remappings: " ++ show (Map.mapKeys identToString (Map.map identToString (remapping st)))

addPrettyToLog :: (Pretty a) => a -> VRState -> VRState
addPrettyToLog o is = let lg = notes is ++ show (pretty o)  ++ "\n" in is { notes = lg }

addToLog :: (Show a) => a -> VRState -> VRState
addToLog o is = let lg = notes is ++ show o ++ "\n" in is { notes = lg }

emptyVRState :: ProgramVersion -> VRState
emptyVRState v = VRState { 
                        notes = "", 
                        version = v,
                        remapping = Map.empty
                        }

getRemapping :: VRTrav VarRemapping
getRemapping = remapping <$> getUserState

getVersion :: VRTrav ProgramVersion
getVersion = version <$> getUserState

versionSuffix :: ProgramVersion -> String
versionSuffix FirstVersion = "_1"
versionSuffix SecondVersion = "_2"

getSuffix :: VRTrav String
getSuffix = versionSuffix <$> getVersion

withRemapping :: (VarRemapping -> VarRemapping) -> VRTrav ()
withRemapping f = modifyUserState (\st -> st { remapping = f (remapping st) })

remapProgram :: ProgramVersion -> CTranslUnit -> CTranslUnit
remapProgram version (CTranslUnit decls node) = 
    let decls' = map (remapExtDecl version) decls
    in CTranslUnit decls' node

remapExtDecl :: ProgramVersion -> CExtDecl -> CExtDecl
remapExtDecl version (CDeclExt decl) = CDeclExt $ remapDecl version decl
remapExtDecl version (CFDefExt fundef) = CFDefExt $ remapFun version fundef
remapExtDecl version asm = asm

remapDecl :: ProgramVersion -> CDecl -> CDecl
remapDecl version d@(CDecl declspecs declrs node) = d
remapDecl version (CStaticAssert expr strLit node) =
    let expr' = remapExpr version expr
    in CStaticAssert expr' strLit node

remapFun :: ProgramVersion -> CFunDef -> CFunDef
remapFun version (CFunDef declspecs declr oldstyle_decls stmt node_info) =
    let declr' = remapDeclr version declr
    in CFunDef declspecs declr' oldstyle_decls stmt node_info

remapDeclr :: ProgramVersion -> CDeclr -> CDeclr
remapDeclr version declr@(CDeclr maybeId derivedDeclrs maybeStrLit attrs node) =
    let suff = versionSuffix version
        id' = appendToId suff <$> maybeId
        derivedDeclrs' = map (remapDerivedDeclr version) derivedDeclrs
    in CDeclr id' derivedDeclrs' maybeStrLit attrs node

remapDerivedDeclr :: ProgramVersion -> CDerivedDeclr -> CDerivedDeclr
remapDerivedDeclr version (CFunDeclr params attrs node) =
    let appSuff = appendToId (versionSuffix version)
        params' = case params of 
                    Left idents -> Left $ map appSuff idents
                    Right (decls, isVariadic) -> Right (map (remapDecl version) decls, isVariadic)
    in CFunDeclr params' attrs node
remapDerivedDeclr version derDeclr = derDeclr
    

remapExpr :: ProgramVersion -> CExpr -> CExpr
remapExpr version = id

-- remapGlobals :: CTranslUnit -> VRTrav ()
-- remapGlobals ast = do
--     withExtDeclHandler (analyseAST ast) remapHandler
--     vRemap <- getRemapping
--     n <- withDefTable (remapDecls vRemap)
--     dTable <- getDefTable
--     log $ map (identToString . declIdent) $ Map.elems $ filterBuiltIns $ gObjs $ globalDefs dTable
--     where
--         remapHandler (DeclEvent de) = do
--             name <- genName
--             suff <- getSuffix
--             let ident@(Ident id _ _) = declIdent de
--                 ident' = mkIdent nopos (id ++ suff) name
--             remap <- getRemapping
--             withRemapping $ Map.insert ident ident'
--         remapHandler _ = return ()

--         remapDecls remap dTable =
--             let globals = Map.mapKeys (\k -> Map.findWithDefault k k remap) $ gObjs $ globalDefs dTable 
--                 globals' = Map.mapWithKey g globals
--                 nsm' = Map.foldrWithKey f nameSpaceMap globals'
--                 f k v nsm = fst $ defGlobal nsm k (Right v)
--                 g k v = editDef k v
--             in (nsm', dTable { identDecls = nsm' })

--         -- TODO: handle typedef etc definitions
--         editDef id (FunctionDef (FunDef (VarDecl _ declattrs ty) stmt node)) =
--             let varname = VarName id Nothing
--             in FunctionDef (FunDef (VarDecl varname declattrs ty) stmt node)
--         editDef id (Declaration (Decl (VarDecl _ declattrs ty) node)) =
--             let varname = VarName id Nothing
--             in Declaration $ Decl (VarDecl varname declattrs ty) node
--         editDef _ def = def

-- remapFunctions :: VRTrav GlobalDecls
-- remapFunctions = do
--     decls <- globalDefs <$> getDefTable
--     objs <- mapM remapObj (gObjs decls)
--     getDefTable >>= (\dt -> unless (inFileScope dt) $ error "Internal Error: Not in filescope after analysis")
--     -- get the global definition table and export to an AST
--     return $ decls { gObjs = objs }

-- remapObj :: IdentDecl -> VRTrav IdentDecl
-- remapObj (FunctionDef (FunDef vardecl body node)) = do
--     body' <- remapFunctionBody node vardecl body
--     return $ FunctionDef $ FunDef vardecl body' node
-- remapObj d = return d

-- remapFunctionBody :: NodeInfo -> VarDecl -> Stmt -> VRTrav Stmt
-- remapFunctionBody node_info decl s@(CCompound localLabels items node_info_body) =
--     do 
--         enterFunctionScope
--         localLabels' <- mapM remapIdent localLabels
--         labelsS <- mapM remapIdent $ getLabels s
--         mapM_ (withDefTable . defineLabel) (localLabels' ++ labelsS)
--         defineParams node_info decl
--         items' <- mapM (remapBlockItem [FunCtx decl]) items
--         leaveFunctionScope
--         return $ CCompound localLabels' items' node_info_body
-- remapFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

-- remapIdent :: Ident -> VRTrav Ident
-- remapIdent (Ident s _ node) = do
--     suf <- getSuffix
--     name <- genName
--     let s' = s ++ suf
--         pos = posOfNode node
--     return $ mkIdent pos s' name

-- remapBlockItem :: [StmtCtx] -> CBlockItem -> VRTrav CBlockItem
-- remapBlockItem _ (CBlockDecl d) = return $ CBlockDecl d -- <$> instrumentDecl True d 
-- remapBlockItem _ (CNestedFunDef fundef) = return $ CNestedFunDef fundef
--     -- CNestedFunDef <$> 
--     --     if shouldInstrumentFunction fundef 
--     --     then instrumentFunction fundef
--     --     else analyseFunDef fundef >> return fundef
-- remapBlockItem c (CBlockStmt s) = return $ CBlockStmt s --CBlockStmt <$> instrumentStmt c s 