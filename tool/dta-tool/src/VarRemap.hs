{-# LANGUAGE FlexibleInstances #-}

module VarRemap(
ProgramVersion(..),
VRState,
    remapping,
emptyVRState,
VRTrav,
remapGlobals,
)
where
import Prelude hiding (log)

import Utils
import Loggable

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Analysis.TravMonad
import Language.C.Analysis.AstAnalysis
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

getSuffix :: VRTrav String
getSuffix = getVersion >>=
    \v -> case v of
        FirstVersion -> return "_1"
        SecondVersion -> return "_2"

withRemapping :: (VarRemapping -> VarRemapping) -> VRTrav ()
withRemapping f = modifyUserState (\st -> st { remapping = f (remapping st) })

remapGlobals :: CTranslUnit -> VRTrav ()
remapGlobals ast = do
    withExtDeclHandler (analyseAST ast) remapHandler
    return () -- TODO: use withDefTable to remap global decls
    where
        remapHandler (DeclEvent de) = do
            name <- genName
            suff <- getSuffix
            let ident@(Ident id _ _) = declIdent de
                ident' = mkIdent nopos (id ++ suff) name
            remap <- getRemapping
            withRemapping $ Map.insert ident ident'
        remapHandler _ = return ()

remapFunction :: CFunDef -> VRTrav CFunDef
remapFunction = undefined

remapProgram :: String -> CTranslUnit -> VRTrav GlobalDecls
remapProgram fnName ast = do
        name <- genName
        dTable <- getDefTable
        let fIdent = mkIdent nopos fnName name
            fDef = fromJust $ getFunDef dTable fIdent
        remapGlobals ast
        fDef' <- remapFunction fDef
        let fDecl = IdentDecl $ FunctionDef fDef'
        withDefTable (defineGlobalIdent fIdent fDecl)
        globalDefs <$> getDefTable

    


