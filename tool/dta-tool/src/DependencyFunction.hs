module DependencyFunction(
getDependencyFunction
)
where
import Prelude hiding (log)

import EntropicDependency
import InstrumentationTrav

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Analysis.TypeUtils
import Language.C.Analysis.SemRep
import Language.C.Analysis.TravMonad
import Language.C.Analysis.DefTable

import Data.List

getDependencyFunction :: OpDependency -> Type -> [Type] -> InstTrav Ident
getDependencyFunction opDep returnType argTypes = do
    fns <- case opDep of
            Preserving -> getPreservingFns 
            Nonpreserving -> getNonPreservingFns 
    let existingFn = find (matchFun returnType argTypes) fns
    case existingFn of
        Just fn -> logPretty fn
        Nothing -> log "no function found"
    case existingFn of
        Just fn -> return $ declIdent fn
        Nothing -> do
            let numFns = length fns
                fnName = case opDep of
                            Preserving -> "preserving" ++ show numFns
                            Nonpreserving -> "nonpreserving" ++ show numFns
            nodeName <- genName
            let fn = makeDependencyFunction nodeName fnName returnType argTypes
            case opDep of
                Preserving -> addPreservingFn fn
                Nonpreserving -> addNonPreservingFn fn
            let id = declIdent fn
            withDefTable $ defineGlobalIdent id (Declaration fn)
            return id
    where matchFun returnType argTypes fundecl =
            let funType = makeFunType returnType argTypes
            in sameType funType (declType fundecl)

makeDependencyFunction :: Name -> String -> Type -> [Type] -> Decl
makeDependencyFunction name fnName returnType argTypes = do
    let pos = nopos
        id = mkIdent pos fnName name
        nodeInfo = mkNodeInfo pos name
        attrs = noAttributes
        declattrs = DeclAttrs noFunctionAttrs (FunLinkage ExternalLinkage) attrs
        functionType = makeFunType returnType argTypes 
        varname = VarName id Nothing
        vardecl = VarDecl varname declattrs functionType
    Decl vardecl nodeInfo

makeFunType :: Type -> [Type] -> Type
makeFunType returnType argTypes =
    let funtype = FunType returnType params False
        params = map paramFn argTypes
        paramFn t = ParamDecl (VarDecl NoName (DeclAttrs noFunctionAttrs NoStorage noAttributes) t) undefNode
        attrs = noAttributes
    in FunctionType funtype attrs