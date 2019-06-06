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
type VarRemapping = NameSpaceMap Ident Ident

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
                "Variable Remappings: " ++ show (Map.mapKeys identToString (Map.map identToString (globalNames (remapping st))))

addPrettyToLog :: (Pretty a) => a -> VRState -> VRState
addPrettyToLog o is = let lg = notes is ++ show (pretty o)  ++ "\n" in is { notes = lg }

addToLog :: (Show a) => a -> VRState -> VRState
addToLog o is = let lg = notes is ++ show o ++ "\n" in is { notes = lg }

emptyVRState :: ProgramVersion -> VRState
emptyVRState v = VRState { 
                        notes = "", 
                        version = v,
                        remapping = nameSpaceMap
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

defGlobalRemap :: Ident -> Ident -> VarRemapping -> VarRemapping
defGlobalRemap id1 id2 vRemap = fst $ defGlobal vRemap id1 id2

remapProgram :: CTranslUnit -> VRTrav GlobalDecls
remapProgram ast = do
        remapGlobals ast
        varRemap <- getRemapping
        suff <- getSuffix
        withDefTable (remapDefTable varRemap suff) 
        globalDefs <$> getDefTable

remapGlobals :: CTranslUnit -> VRTrav ()
remapGlobals ast = do
    withExtDeclHandler (analyseAST ast) remapHandler
    return ()
    where
        remapHandler (DeclEvent de) = do
            name <- genName
            suff <- getSuffix
            let ident = declIdent de
                ident' = appendToIdent name suff ident
            remap <- getRemapping
            withRemapping $ defGlobalRemap ident ident' 
        remapHandler _ = return ()

remapDefTable varRemap suff dTable =
    let remap = globalNames varRemap
        globals = Map.mapKeys (\k -> Map.findWithDefault k k remap) $ gObjs $ globalDefs dTable 
        globals' = Map.mapWithKey (editDef varRemap suff) globals
        nsm' = Map.foldrWithKey f nameSpaceMap globals'
        f k v nsm = fst $ defGlobal nsm k (Right v)
    in ((), dTable { identDecls = nsm' })
    where

    -- TODO: handle typedef etc definitions
    editDef :: VarRemapping -> String -> Ident -> IdentDecl -> IdentDecl
    editDef varRemap suff id (FunctionDef (FunDef (VarDecl _ declattrs ty) stmt node)) =
        let varname = VarName id Nothing
            ty' = editType id ty
            stmt = remapFunctionBody
        in FunctionDef (FunDef (VarDecl varname declattrs ty) stmt node)
    editDef varRemap suff id (Declaration (Decl (VarDecl _ declattrs ty) node)) =
        let varname = VarName id Nothing
        in Declaration $ Decl (VarDecl varname declattrs ty) node
    editDef _ _ _ def = def

    editType id (FunctionType (FunType ty params isVariadic) attrs) = 
        let params' = map (editParamDecl id) params
        in FunctionType (FunType ty params' isVariadic) attrs
    editType _ t = t

    editParamDecl id (ParamDecl vd ni) = ParamDecl (remapVarDecl id vd) ni
    editParamDecl id (AbstractParamDecl vd ni) = AbstractParamDecl (remapVarDecl id vd) ni


remapVarDecl :: Ident -> VarDecl -> VarDecl
remapVarDecl id (VarDecl _ declattrs ty) = VarDecl (VarName id Nothing) declattrs ty

remapFunctions :: VRTrav GlobalDecls
remapFunctions = do
    decls <- globalDefs <$> getDefTable
    objs <- mapM remapObj (gObjs decls)
    getDefTable >>= (\dt -> unless (inFileScope dt) $ error "Internal Error: Not in filescope after analysis")
    -- get the global definition table and export to an AST
    return $ decls { gObjs = objs }

remapObj :: IdentDecl -> VRTrav IdentDecl
remapObj (FunctionDef (FunDef vardecl body node)) = do
    body' <- remapFunctionBody node vardecl body
    return $ FunctionDef $ FunDef vardecl body' node
remapObj d = return d

remapFunctionBody :: NodeInfo -> VarDecl -> Stmt -> VRTrav Stmt
remapFunctionBody node_info decl s@(CCompound localLabels items node_info_body) =
    do 
        enterFunctionScope
        localLabels' <- mapM remapIdent localLabels
        labelsS <- mapM remapIdent $ getLabels s
        mapM_ (withDefTable . defineLabel) (localLabels' ++ labelsS)
        defineParams node_info decl
        items' <- mapM (remapBlockItem [FunCtx decl]) items
        leaveFunctionScope
        return $ CCompound localLabels' items' node_info_body
remapFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"

remapIdent :: Ident -> VRTrav Ident
remapIdent (Ident s _ node) = do
    suf <- getSuffix
    name <- genName
    let s' = s ++ suf
        pos = posOfNode node
    return $ mkIdent pos s' name

remapBlockItem :: [StmtCtx] -> CBlockItem -> VRTrav CBlockItem
remapBlockItem _ (CBlockDecl d) = return $ CBlockDecl d -- <$> instrumentDecl True d 
remapBlockItem _ (CNestedFunDef fundef) = return $ CNestedFunDef fundef
    -- CNestedFunDef <$> 
    --     if shouldInstrumentFunction fundef 
    --     then instrumentFunction fundef
    --     else analyseFunDef fundef >> return fundef
remapBlockItem c (CBlockStmt s) = return $ CBlockStmt s --CBlockStmt <$> instrumentStmt c s 