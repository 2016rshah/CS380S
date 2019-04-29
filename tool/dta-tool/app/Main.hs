module Main where

import Utils
import Instrumentation
import VarRemap

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
        fp <- head <$> getArgs
        parseResult <- parseCFile (newGCC "gcc") Nothing [] fp
        let parsed@(CTranslUnit ed ni) = resultOrDie parseResult
        return ()
        --     (traversalResult, traversalState) = runTravOrDie (emptyVRState FirstVersion) $ remapGlobals parsed
        -- print $ userState traversalState


--  intstrumentation main
-- main :: IO ()
-- main = do
--         fp <- head <$> getArgs
--         parseResult <- parseCFile (newGCC "gcc") Nothing [] fp
--         let f (Left pr) = error $ show pr
--             f (Right pr) = pr
--             parsed@(CTranslUnit ed ni) = f parseResult
--             g (Left err) = error $ "Traversal error: " ++ show err
--             g (Right (result, state)) = (result, userState state)
--             (traversalResult, traversalState) = g $ runTrav (emptyInstState fp) $ instrumentationTraversal parsed
--         print traversalState
--         let astOutput = prettyAst parsed
--         writeFile "astEdited.hs" astOutput
--         let newAst = case parseC (inputStreamFromString $ show $ pretty traversalResult) (initPos fp) of
--                         (Left parseError) -> error "Parse error"
--                         (Right newTranslUnit) -> newTranslUnit
--         writeFile "globalDecls.hs" $ prettyAst newAst
--         printFormattedFilename fp 18
--         print $ pretty parsed
--         printFormattedFilename (fp ++ " Transformed") 12
--         print $ pretty newAst
--     where
--         prettyAst ast@(CTranslUnit ed ni) = ppShow ed
--         printFormattedFilename fp n = do
--             putStrLn $ "|" ++ replicate (2 * n + length fp) '=' ++ "|"
--             putStrLn $ "|" ++ replicate n ' ' ++ fp ++ replicate n ' ' ++ "|"
--             putStrLn $ "|" ++ replicate (2 * n + length fp) '=' ++ "|"


