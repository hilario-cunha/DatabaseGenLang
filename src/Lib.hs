module Lib
    ( someFunc
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import CSharpGen
import Data.List (intercalate)

data DbFieldType = Varchar Int

cSharpType :: DbFieldType -> [Char]
cSharpType (Varchar _) = "string"

type IsNotNull = Bool
data DbField = DbField String DbFieldType IsNotNull

data DbTable = DbTable String [DbField] [String]

exctractDbFieldsFromTable :: DbTable -> [DbField]
exctractDbFieldsFromTable (DbTable _ dbFields _) = dbFields

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

extractNameFromDbField :: DbField -> String
extractNameFromDbField (DbField name _ _) = name

dbFieldsToSqlValues :: [DbField] -> String
dbFieldsToSqlValues dbFields = intercalate "," $ map (extractNameFromDbField) dbFields

extractParamNameFromDbField :: DbField -> String
extractParamNameFromDbField dbField = "@" ++ extractNameFromDbField dbField

dbFieldsToSqlParams :: [DbField] -> String
dbFieldsToSqlParams dbFields = intercalate "," $ map (extractParamNameFromDbField) dbFields

tableToSqlInsertOrUpdate :: DbTable -> String
tableToSqlInsertOrUpdate (DbTable tableName dbFields _) = "insert or replace into " ++ tableName ++" (" ++ dbFieldsToSqlValues dbFields ++ ") values (" ++ dbFieldsToSqlParams dbFields ++ ")"

tableToSqlReader :: DbTable -> [DbField] -> String
tableToSqlReader (DbTable tableName dbFields _) searchFields = "SELECT " ++ dbFieldsToSqlValues dbFields ++ " From " ++ tableName ++ " Where " ++ whereSql
    where
        whereSql = intercalate "," searchFieldsToSql
        searchFieldsToSql = map searchFieldToSql searchFields
        searchFieldToSql searchField = (extractNameFromDbField searchField) ++ "=" ++ (extractParamNameFromDbField searchField)

someFunc :: IO ()
someFunc = do
    generateTableClasses createTableWithFunctionalityNameForReasons
    generateTableClasses createTableWithFunctionalityNameForZones

createTableWithFunctionalityNameForZones :: TableWithFunctionalityName
createTableWithFunctionalityNameForZones = TableWithFunctionalityName functionalityName zonesTable searchDbFields
    where 
        functionalityName = "Zones"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        featureIdDbField = DbField "FeatureId" (Varchar 50) True
        zonesTable = DbTable 
            tableName
            [ featureIdDbField
            , DbField "ZoneCode" (Varchar 50) True
            , DbField "Value" (Varchar 1000) False
            ]
            [ "FeatureId", "ZoneCode"]
        searchDbFields = [featureIdDbField]

createTableWithFunctionalityNameForReasons :: TableWithFunctionalityName
createTableWithFunctionalityNameForReasons = TableWithFunctionalityName functionalityName reasonsTable searchDbFields
    where 
        functionalityName = "Reasons"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        featureIdDbField = DbField "FeatureId" (Varchar 50) True
        reasonsTable = DbTable 
            tableName
            [ featureIdDbField
            , DbField "ReasonCode" (Varchar 50) True
            , DbField "Value" (Varchar 1000) False
            ]
            [ "FeatureId", "ReasonCode"]
        searchDbFields = [featureIdDbField]

generateTableClasses :: TableWithFunctionalityName -> IO ()
generateTableClasses tableWithFunctionalityName = mapM_ createAndWriteToFileDatabaseTemplate (createNamespaceWithClassesForTable tableWithFunctionalityName)

createNamespaceWithClassesForTable :: TableWithFunctionalityName -> [NamespaceWithClass]
createNamespaceWithClassesForTable tableWithFunctionalityName = 
    [ createNamespaceWithClassForTableCreateTable tableWithFunctionalityName
    , createNamespaceWithClassForTableInsertOrUpdate tableWithFunctionalityName
    , createNamespaceWithClassForTableReader tableWithFunctionalityName
    ]

type SearchDbFields = [DbField]
data TableWithFunctionalityName = TableWithFunctionalityName String DbTable SearchDbFields

createNamespaceWithClassForTableReader :: TableWithFunctionalityName -> NamespaceWithClass
createNamespaceWithClassForTableReader (TableWithFunctionalityName functionalityName table searchDbFields) = NamespaceWithClass 
    { usings = ["System.Data.SQLite", "Tlantic.SQLite"]
    , nameSpace = "MRS.InStore.SDK.SQLite"
    , classDefinition = ClassWithMethods
        { className = cn
        , ctor = mkCtorForReader sectionName ctorName sql searchDbFields
        , methods = [mkReaderMap rowType dbFields]
        }
    }
    where 
        sectionName = functionalityName ++ "Dal"
        ctorName = (functionalityName ++ "Reader")
        rowType = functionalityName ++ "Row";
        cn = ClassWithBase ctorName $ "ExecuteReader<" ++ rowType ++ ">"
        sql = tableToSqlReader table searchDbFields
        dbFields = exctractDbFieldsFromTable table

createNamespaceWithClassForTableInsertOrUpdate :: TableWithFunctionalityName -> NamespaceWithClass
createNamespaceWithClassForTableInsertOrUpdate (TableWithFunctionalityName functionalityName table _) = NamespaceWithClass 
    { usings = ["System.Collections.Generic", "System.Data.SQLite", "Tlantic.SQLite"]
    , nameSpace = "MRS.InStore.SDK.SQLite"
    , classDefinition = ClassWithMethods
        { className = cn
        , ctor = mkCtorForInsertOrUpdate sectionName ctorName sql rowType paramNames
        , methods = [mkUpdateCommandParameters rowType dbFields]
        }
    }
    where 
        sectionName = functionalityName ++ "Dal"
        ctorName = (functionalityName ++ "InsertOrUpdate")
        rowType = functionalityName ++ "Row";
        cn = ClassWithBase ctorName $ "ExecuteNonQueryInBulk<" ++ rowType ++ ">"
        sql = tableToSqlInsertOrUpdate table
        dbFields = exctractDbFieldsFromTable table
        paramNames = map extractParamNameFromDbField dbFields

mkUpdateCommandParameters :: String -> [DbField] -> MemberDeclaration
mkUpdateCommandParameters rowType dbFields =
    mkMethodMemberDeclarationWithVoidReturn [Public, Override] "UpdateCommandParameters" [parametersFormalParam, rowFormalParam] body
    where
        parametersFormalParam = mkFormalParam "SQLiteParameterCollection" "parameters"
        rowFormalParam = mkFormalParam rowType "row"
        fieldNames = map extractNameFromDbField dbFields
        body = map assign fieldNames
        assign fieldName = ExpressionStatement $ mkAssign (parametersAccess fieldName) (rowAccess fieldName)
        rowAccess fieldName = MemberAccess (mkPrimaryMemberAccess (mkSimpleName "row") fieldName)
        parametersAccess fieldName = MemberAccess (mkPrimaryMemberAccess (ElementAccess (mkSimpleName "parameters") [mkLiteralString ("@" ++ fieldName)]) "Value")

mkReaderMap :: String -> [DbField] -> MemberDeclaration
mkReaderMap rowType dbFields =
    mkMethodMemberDeclaration [Protected, Override] (mkTypeNamed rowType) "Map" [dataReaderFormalParam] body
    where
        dataReaderFormalParam = mkFormalParam "CustomSQLiteDataReader" "dr"
        body = [mkReturn $ mkNew rowType bodyArgs]
        bodyArgs = map mkArgument rowTypeValues
        dataReaderGetAs (Varchar _) fieldName = mkInvocationSimpleName "dr.GetAsString" [mkLiteralStringArgument fieldName]
        dataReaderGetAsDbField (DbField name dbFieldType _) = dataReaderGetAs dbFieldType name
        rowTypeValues = map dataReaderGetAsDbField dbFields
        -- dr.GetAsString("FeatureId"), dr.GetAsString("ZoneCode"), dr.GetAsString("Value")

createNamespaceWithClassForTableCreateTable :: TableWithFunctionalityName -> NamespaceWithClass
createNamespaceWithClassForTableCreateTable (TableWithFunctionalityName functionalityName table _) = NamespaceWithClass 
    { usings = ["Tlantic.SQLite"]
    , nameSpace = "MRS.InStore.SDK.SQLite"
    , classDefinition = ClassWithMethods
        { className = cn
        , ctor = mkCtorForCreateTable sectionName ctorName sql
        , methods = []
        }
    }
    where 
        sectionName = functionalityName ++ "Dal"
        ctorName = (functionalityName ++ "CreateTable")
        cn = ClassWithBase ctorName "ExecuteNonQuery"
        sql = tableToSqlCreateTable table

createAndWriteToFileDatabaseTemplate :: NamespaceWithClass -> IO ()
createAndWriteToFileDatabaseTemplate templateData  = 
    let 
        getClassName (Class cn) = cn
        getClassName (ClassWithBase cn _) = cn

        createServerRequestsFile = templateData
        fileName = "output/" ++ (getClassName (className (classDefinition createServerRequestsFile))) ++ ".DatabaseGen.cs"
        cu = mkCu createServerRequestsFile
    in createAndWriteToFile fileName cu

mkCu :: NamespaceWithClass -> CompilationUnit
mkCu = mkNamespaceWithClass mkTemplateSimpleGetClass

mkClassWithBaseClass :: [Modifier] -> String -> String -> [MemberDeclaration] -> Declaration
mkClassWithBaseClass modifiers cn bn cb = TypeDeclaration (ClassTypeDeclaration [] modifiers (Identifier cn) [] [TypeName (Name [Identifier bn]) []] [] (ClassBody cb))

mkTemplateSimpleGetClass :: ClassWithMethods -> Declaration
mkTemplateSimpleGetClass classWithMethods = 
    mkClass (className classWithMethods) (mkTemplateSimpleGetClassBody classWithMethods)
    where 
        mkClass (Class cn)= mkPublicClass cn
        mkClass (ClassWithBase cn bn)= mkClassWithBaseClass [Public] cn bn

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

mkCtorForReader :: String -> String -> String -> [DbField] -> MemberDeclaration
mkCtorForReader sectionName ctorName sql dbFieldsToSearch = 
    mkConstructorMemberDeclarationWithConstructorInitializer [Public] ctorName (dbFormalParam:paramsFormalParams) (ConstructorBaseCall baseCallArguments) []
    where
        baseCallArguments = [dbArgument,classNameToShowInLogArgument,methodNameToShowInLogArgument, sqlArgument] ++ paramNamesArguments
        dbName = "db"
        dbFormalParam = mkFormalParam "SQLiteDb"  dbName
        paramsFormalParams = map paramFormalParam dbFieldsToSearch
        paramFormalParam (DbField name dbFieldType _) = mkFormalParam (cSharpType dbFieldType) (camelCase name)
        dbArgument = mkArgument $ mkSimpleName dbName
        classNameToShowInLogArgument =  mkLiteralStringArgument sectionName
        methodNameToShowInLogArgument =  mkLiteralStringArgument ctorName
        sqlArgument = mkLiteralStringArgument sql
        paramNames = map extractNameFromDbField dbFieldsToSearch
        paramNamesArguments = map paramNameArgument paramNames
        paramNameArgument fieldName = mkNewArgument "SQLiteParameter" [mkLiteralStringArgument ("@" ++ fieldName), mkSimpleNameArgument (camelCase fieldName)]

mkCtorForInsertOrUpdate :: String -> String -> String -> String -> [String] -> MemberDeclaration
mkCtorForInsertOrUpdate sectionName ctorName sql rowsType paramNames = 
    mkConstructorMemberDeclarationWithConstructorInitializer [Public] ctorName [dbFormalParam, rowsFormalParam] (ConstructorBaseCall baseCallArguments) []
    where
        baseCallArguments = [dbArgument,classNameToShowInLogArgument,methodNameToShowInLogArgument,rowsArgument,sqlArgument] ++ paramNamesArguments
        dbName = "db"
        dbFormalParam = mkFormalParam "SQLiteDb" dbName
        rowsName = "rows"
        rowsFormalParam = mkFormalParam ("IEnumerable<" ++ rowsType ++ ">") rowsName
        dbArgument = mkArgument $ mkSimpleName dbName
        classNameToShowInLogArgument =  mkLiteralStringArgument sectionName
        methodNameToShowInLogArgument =  mkLiteralStringArgument ctorName
        rowsArgument = mkArgument $ mkSimpleName rowsName
        sqlArgument = mkLiteralStringArgument sql
        paramNamesArguments = map paramNameArgument paramNames
        paramNameArgument paramName = mkNewArgument "SQLiteParameter" [mkLiteralStringArgument paramName]

createAndWriteToFile :: Pretty a => FilePath -> a -> IO ()
createAndWriteToFile fileName cu  = writeFile fileName $ prettyPrint cu      
