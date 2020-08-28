module Gen
    ( NamespaceWithClass(..)
    , ClassWithMethods(..)
    , createNamespaceWithClass
    , createClassWithMethods
    , mkNamespaceWithClass
    ) where

import Language.CSharp.Syntax
import CSharpGen

data NamespaceWithClass = NamespaceWithClass
    { usings :: [String]
    , nameSpace :: String
    , classDefinition :: ClassWithMethods
    }

data ClassWithMethods = ClassWithMethods
    { className :: String
    , ctor :: MemberDeclaration
    , methods :: [MemberDeclaration]
    }

createNamespaceWithClass :: [String] -> String -> ClassWithMethods -> NamespaceWithClass
createNamespaceWithClass u n c =  NamespaceWithClass
    { usings = u
    , nameSpace = n
    , classDefinition = c
    }

createClassWithMethods :: String -> MemberDeclaration -> [MemberDeclaration] -> ClassWithMethods
createClassWithMethods cn c m = ClassWithMethods
    { className = cn
    , ctor = c
    , methods = m
    }

mkNamespaceWithClass :: (ClassWithMethods -> Declaration) -> NamespaceWithClass -> CompilationUnit
mkNamespaceWithClass createClassFromDefinition namespaceWithClass = 
    CompilationUnit 
        (mkUsings $ usings namespaceWithClass) 
        [mkNamespace (nameSpace namespaceWithClass) $ [createClassFromDefinition (classDefinition namespaceWithClass)]]
