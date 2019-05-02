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
import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Analysis.TravMonad
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.NameSpaceMap
import Language.C.Analysis.SemRep
import Language.C.Analysis.DefTable
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

remapGlobals :: CTranslUnit -> VRTrav ()
remapGlobals ast = do
    withExtDeclHandler (analyseAST ast) remapHandler
    vRemap <- getRemapping
    n <- withDefTable (remapDecls vRemap)
    dTable <- getDefTable
    log $ map (identToString . declIdent) $ Map.elems $ filterBuiltIns $ gObjs $ globalDefs dTable
    where
        remapHandler (DeclEvent de) = do
            name <- genName
            suff <- getSuffix
            let ident@(Ident id _ _) = declIdent de
                ident' = mkIdent nopos (id ++ suff) name
            remap <- getRemapping
            withRemapping $ Map.insert ident ident'
        remapHandler _ = return ()

        remapDecls remap dTable =
            let globals = Map.mapKeys (\k -> Map.findWithDefault k k remap) $ gObjs $ globalDefs dTable 
                globals' = Map.mapWithKey g globals
                nsm' = Map.foldrWithKey f nameSpaceMap globals'
                f k v nsm = fst $ defGlobal nsm k (Right v)
                g k v = editDef k v
            in (nsm', dTable { identDecls = nsm' })

        editDef id (FunctionDef (FunDef (VarDecl _ declattrs ty) stmt node)) =
            let varname = VarName id Nothing
            in FunctionDef (FunDef (VarDecl varname declattrs ty) stmt node)
        editDef id (Declaration (Decl (VarDecl _ declattrs ty) node)) =
            let varname = VarName id Nothing
            in Declaration $ Decl (VarDecl varname declattrs ty) node
        editDef _ def = def


remapFunctions :: VRTrav GlobalDecls
remapFunctions = do
    decls <- globalDefs <$> getDefTable
    mapM_ remapObj (gObjs decls)
    getDefTable >>= (\dt -> unless (inFileScope dt) $ error "Internal Error: Not in filescope after analysis")
    -- get the global definition table and export to an AST
    globalDefs <$> getDefTable 

remapObj :: IdentDecl -> VRTrav ()
remapObj (FunctionDef fundef) = return ()
remapObj _ = return ()

remapProgram :: CTranslUnit -> VRTrav GlobalDecls
remapProgram ast = do
        remapGlobals ast
        remapFunctions 