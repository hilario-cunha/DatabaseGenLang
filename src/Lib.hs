module Lib
    ( someFunc
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import CSharpGen
import Data.List (intercalate)

data DbFieldType = Varchar Int

type IsNotNull = Bool
data DbField = DbField String DbFieldType IsNotNull

data DbTable = DbTable String [DbField] [String]

primaryKeyToSql :: [String] -> String
primaryKeyToSql keys = "PRIMARY KEY(" ++ intercalate "," keys ++ ")"
fieldTypeToSql :: DbFieldType -> String
fieldTypeToSql (Varchar s) = "VARCHAR(" ++ show s ++ ")"
isNotNullToSql :: Bool -> String
isNotNullToSql isNotNull = if(isNotNull) then "NOT NULL" else "NULL"
fieldToSql :: DbField -> String
fieldToSql (DbField name fieldType isNotNull) = intercalate " " [name, fieldTypeToSql fieldType, isNotNullToSql isNotNull]
fieldsToSql :: [DbField] -> String
fieldsToSql dbFields = intercalate "," $ map fieldToSql dbFields
tableToSqlCreateTable :: DbTable -> String
tableToSqlCreateTable (DbTable tableName dbFields primaryKeys) =  "CREATE TABLE " ++ tableName ++ "(" ++ tableBody primaryKeys ++ ")"
    where 
        tableBody [] = fieldsToSql dbFields
        tableBody keys = fieldsToSql dbFields ++ "," ++ primaryKeyToSql keys

someFunc :: IO ()
someFunc = do
    createAndWriteToFileDatabaseTemplate $ createNamespaceWithClassForTableZones
    createAndWriteToFileDatabaseTemplate $ createNamespaceWithClassForTableReasons


createNamespaceWithClassForTableZones :: NamespaceWithClass
createNamespaceWithClassForTableZones = createNamespaceWithClassForTable functionalityName zonesTable
    where 
        functionalityName = "Zones"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        zonesTable = DbTable 
            tableName
            [ DbField "FeatureId" (Varchar 50) True
            , DbField "ZoneCode" (Varchar 50) True
            , DbField "Value" (Varchar 1000) False
            ]
            [ "FeatureId", "ZoneCode"]

createNamespaceWithClassForTableReasons :: NamespaceWithClass
createNamespaceWithClassForTableReasons = createNamespaceWithClassForTable functionalityName zonesTable
    where 
        functionalityName = "Reasons"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        zonesTable = DbTable 
            tableName
            [ DbField "FeatureId" (Varchar 50) True
            , DbField "ReasonCode" (Varchar 50) True
            , DbField "Value" (Varchar 1000) False
            ]
            [ "FeatureId", "ReasonCode"]

createNamespaceWithClassForTable :: String -> DbTable -> NamespaceWithClass
createNamespaceWithClassForTable functionalityName table = NamespaceWithClass 
    { usings = ["Tlantic.SQLite"]
    , nameSpace = "MRS.InStore.SDK.SQLite"
    , classDefinition = ClassWithMethods
        { className = cn
        , ctor = mkCtorForCreateTable sectionName cn sql
        , methods = []
        }
    }
    where 
        sectionName = functionalityName ++ "Dal"
        cn = functionalityName ++ "CreateTable"
        sql = tableToSqlCreateTable table

createAndWriteToFileDatabaseTemplate :: NamespaceWithClass -> IO ()
createAndWriteToFileDatabaseTemplate templateData  = 
    let 
        createServerRequestsFile = templateData
        fileName = "output/" ++ (className (classDefinition createServerRequestsFile)) ++ ".DatabaseGen.cs"
        cu = mkCu createServerRequestsFile
    in createAndWriteToFile fileName cu

mkCu :: NamespaceWithClass -> CompilationUnit
mkCu = mkNamespaceWithClass mkTemplateSimpleGetClass

mkClassWithBaseClass :: [Modifier] -> String -> String -> [MemberDeclaration] -> Declaration
mkClassWithBaseClass modifiers cn bn cb = TypeDeclaration (ClassTypeDeclaration [] modifiers (Identifier cn) [] [TypeName (Name [Identifier bn]) []] [] (ClassBody cb))

mkTemplateSimpleGetClass :: ClassWithMethods -> Declaration
mkTemplateSimpleGetClass classWithMethods = 
    mkClassWithBaseClass [Public] cn "ExecuteNonQuery" cb
    where 
        cn = className classWithMethods
        cb = mkTemplateSimpleGetClassBody classWithMethods

mkTemplateSimpleGetClassBody :: ClassWithMethods -> [MemberDeclaration]
mkTemplateSimpleGetClassBody classWithMethods = (ctor1 : ms)
    where 
        ctor1 = ctor classWithMethods
        ms = methods classWithMethods

mkCtorForCreateTable :: String -> String -> String -> MemberDeclaration
mkCtorForCreateTable sectionName ctorName sql = 
    mkConstructorMemberDeclarationWithConstructorInitializer [Public] ctorName [dbFormalParam] (ConstructorBaseCall baseCallArguments) []
    where
        baseCallArguments = [dbArgument,classNameToShowInLogArgument,methodNameToShowInLogArgument,sqlArgument]
        dbName = "db"
        dbFormalParam = mkFormalParam "SQLiteDb" dbName
        dbArgument = mkArgument $ mkSimpleName dbName
        classNameToShowInLogArgument =  mkLiteralStringArgument sectionName
        methodNameToShowInLogArgument =  mkLiteralStringArgument ctorName
        sqlArgument = mkLiteralStringArgument sql

createAndWriteToFile :: Pretty a => FilePath -> a -> IO ()
createAndWriteToFile fileName cu  = writeFile fileName $ prettyPrint cu      
