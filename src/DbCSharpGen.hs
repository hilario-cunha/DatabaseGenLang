module DbCSharpGen 
    ( createNamespaceWithClassForTableCreateTable
    , createNamespaceWithClassForTableInsertOrUpdate
    , createNamespaceWithClassForTableReader
    , createNamespaceWithClassForRow
    ) where

import Db
import Gen
import Language.CSharp.Syntax
import CSharpGen

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

createNamespaceWithClassForRow :: TableWithFunctionalityName -> NamespaceWithClass
createNamespaceWithClassForRow (TableWithFunctionalityName functionalityName table _) = NamespaceWithClass 
    { usings = []
    , nameSpace = "MRS.InStore.SDK.SQLite"
    , classDefinition = ClassWithMethods
        { className = cn
        , ctor = mkCtorForRow ctorName dbFields
        , methods = createProperties dbFields
        }
    }
    where 
        ctorName = (functionalityName ++ "Row")
        cn = Class ctorName
        dbFields = exctractDbFieldsFromTable table

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

mkCtorForRow :: String -> [DbField] -> MemberDeclaration
mkCtorForRow ctorName dbFields = 
    mkConstructorMemberDeclaration [Public] ctorName paramsFormalParams mkAssignments
    where
        paramsFormalParams = map paramFormalParam dbFields
        paramFormalParam (DbField name dbFieldType _) = mkFormalParam (cSharpType dbFieldType) (camelCase name)
        mkAssignments = map mkAssignmentP dbFields
        mkAssignmentP (DbField n _ _) = ExpressionStatement $ mkAssignThisDot n (camelCase n)

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

createProperties :: [DbField] -> [MemberDeclaration]
createProperties dbFields = map mkAutoProperty dbFields
    where 
        mkAutoProperty (DbField name dbFieldType _) = mkPropertyAutoPublicGet (cSharpType dbFieldType) name
