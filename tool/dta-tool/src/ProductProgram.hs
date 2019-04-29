module ProductProgram(
productProgram
)
where

import Utils
import VarRemap

import Language.C.Syntax.AST
import Language.C.Analysis.DefTable
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Name

productProgram :: String -> CTranslUnit -> CTranslUnit -> CTranslUnit
productProgram fnName ast1 ast2 =
    let 
        g1 = runTravOrDie (emptyVRState FirstVersion) $ remapProgram fnName ast1
        g2 = runTravOrDie (emptyVRState SecondVersion) $ remapProgram fnName ast2
        (fDef1, g1') = extractFunction g1 fnName
        (fDef2, g2') = extractFunction g2 fnName
        g = mergeGlobalDecls g1' g2'
        f = prodProg fDef1 fDef2
        g' = g { gObjs = Map.insert fnName f g }
        export g'

prodProg :: FunDef -> FunDef -> FunDef
prodProg = undefined

extractFunction :: GlobalDecls -> String -> (FunDef, GlobalDecls)
extractFunction = undefined