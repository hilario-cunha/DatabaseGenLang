module Lib
    ( someFunc
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import CSharpGen

someFunc :: IO ()
someFunc = createAndWriteToFileDatabaseTemplate $ NamespaceWithClass 
    { usings = []
    , nameSpace = "ns"
    , classDefinition = ClassWithMethods
        { className = "classname"
        , ctor = mkTemplateSimpleGetCtor "classname"
        , methods = []
        }
    }

createAndWriteToFileDatabaseTemplate :: NamespaceWithClass -> IO ()
createAndWriteToFileDatabaseTemplate templateData  = 
    let 
        createServerRequestsFile = templateData
        fileName = "output/" ++ (className (classDefinition createServerRequestsFile)) ++ ".DatabaseGen.cs"
        cu = mkCu createServerRequestsFile
    in createAndWriteToFile fileName cu

mkCu :: NamespaceWithClass -> CompilationUnit
mkCu = mkNamespaceWithClass mkTemplateSimpleGetClass

mkTemplateSimpleGetClass :: ClassWithMethods -> Declaration
mkTemplateSimpleGetClass classWithMethods = 
    mkPublicClass cn cb
    where 
        cn = className classWithMethods
        cb = mkTemplateSimpleGetClassBody cn classWithMethods

mkTemplateSimpleGetClassBody :: [Char] -> ClassWithMethods -> [MemberDeclaration]
mkTemplateSimpleGetClassBody ctorName classWithMethods = (mkServerConfigField : ctor1 : ms)
    where 
        mkServerConfigField = mkField "ServerConfig" "serverConfig"
        ctor1 = mkTemplateSimpleGetCtor ctorName
        ms = methods classWithMethods

mkTemplateSimpleGetCtor :: String -> MemberDeclaration
mkTemplateSimpleGetCtor ctorName = mkConstructorMemberDeclaration [Internal] ctorName [serverConfigFormalParam] [serverConfigAssign]
    where
        serverConfigFormalParam = mkFormalParam "ServerConfig" "serverConfig"
        serverConfigAssign = mkAssignStatement "this.serverConfig" "serverConfig"

createAndWriteToFile :: Pretty a => FilePath -> a -> IO ()
createAndWriteToFile fileName cu  = writeFile fileName $ prettyPrint cu      
