module Main where

import Utils
import Instrumentation
import VarRemap
import ProductProgram

import Language.C
import Language.C.Syntax.AST
import Language.C.Analysis.AstAnalysis
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

main :: IO ()
main = do
        fp1 <- (!! 0) <$> getArgs
        fp2 <- (!! 1) <$> getArgs
        parseResult1 <- parseCFile (newGCC "gcc") Nothing [] fp1
        parseResult2 <- parseCFile (newGCC "gcc") Nothing [] fp2
        let parsed1 = resultOrDie parseResult1
            parsed2 = resultOrDie parseResult2
            instrumentedAst1 = instrumentation parsed1
            instrumentedAst2 = instrumentation parsed2
        pp <- productProgram "main1" instrumentedAst1 instrumentedAst2
        print $ pretty pp
