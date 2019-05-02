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
import Language.C.Analysis.Export

import qualified Data.Map as Map

productProgram :: String -> CTranslUnit -> CTranslUnit -> CTranslUnit
productProgram fnName ast1 ast2 =
    let 
        g1 = runTravOrDie_ (emptyVRState FirstVersion) $ remapProgram ast1
        g2 = runTravOrDie_ (emptyVRState SecondVersion) $ remapProgram ast2
        fId = mkIdent nopos fnName (Name 0)
        (fDef1, g1') = extractFunction g1 fnName
        (fDef2, g2') = extractFunction g2 fnName
        g = mergeGlobalDecls g1' g2'
        f = prodProg fnName fDef1 fDef2
        funDecl = FunctionDef f
        g' = g { gObjs = Map.insert fId funDecl (gObjs g) }
    in
        export g'

prodProg :: String -> FunDef -> FunDef -> FunDef
prodProg = undefined

extractFunction :: GlobalDecls -> String -> (FunDef, GlobalDecls)
extractFunction = undefined