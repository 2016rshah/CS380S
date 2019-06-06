module ProductProgram(
productProgram
)
where

import Utils
import VarRemap

import Language.C.Syntax.AST
import Language.C.Analysis.DefTable
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Name
import Language.C.Analysis.Export

import qualified Data.Map as Map
import Control.Arrow

productProgram :: String -> CTranslUnit -> CTranslUnit -> IO CTranslUnit
productProgram fnName ast1 ast2 =
    let 
        (g1, s1) = runTravOrDie (emptyVRState FirstVersion) $ remapProgram ast1
        -- (g2, s2) = runTravOrDie (emptyVRState SecondVersion) $ remapProgram ast2
        -- fId = mkIdent nopos fnName (Name 0)
        -- (fDef1, g1') = extractFunction g1 $ fnName ++ versionSuffix FirstVersion
        -- (fDef2, g2') = extractFunction g2 $ fnName ++ versionSuffix SecondVersion
        -- g = mergeGlobalDecls g1' g2'
        g = g1
        -- f = prodProg fnName fDef1 fDef2
        -- funDecl = FunctionDef f
    in do
        print $ userState s1
        print $ map identToString $ Map.keys $ filterBuiltIns $ gObjs $ g
        return $ export $ g -- { gObjs = Map.insert fId funDecl (gObjs g) }

prodProg :: String -> FunDef -> FunDef -> FunDef
prodProg fnName _ _ = emptyFunDef fnName

extractFunction :: GlobalDecls -> String -> (FunDef, GlobalDecls)
extractFunction decls fnName = 
    let fId = mkIdent nopos fnName (Name 0)
        f (Just (FunctionDef fundef)) = fundef
        f _ = error "Could not extract function"
        g objs = decls { gObjs = objs }
        (funDecl, objs') = Map.updateLookupWithKey (const . const Nothing) fId $ gObjs decls
    in (f funDecl, g objs')