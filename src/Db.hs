module Db
    ( TableWithFunctionalityName(..)
    , DbTable(..)
    , DbField(..)
    , DbFieldType(..)
    , cSharpType
    , exctractDbFieldsFromTable
    , extractParamNameFromDbField
    , extractNameFromDbField
    , tableToSqlCreateTable
    , tableToSqlInsertOrUpdate
    , tableToSqlReader
    ) where

import Data.List (intercalate)
data TableWithFunctionalityName = TableWithFunctionalityName String DbTable SearchDbFields

data DbTable = DbTable String [DbField] [String]

data DbField = DbField String DbFieldType IsNotNull
data DbFieldType    = Varchar Int
                    | DbInt
type IsNotNull = Bool

type SearchDbFields = [DbField]

cSharpType :: DbFieldType -> String
cSharpType (Varchar _) = "string"
cSharpType (DbInt) = "int"

exctractDbFieldsFromTable :: DbTable -> [DbField]
exctractDbFieldsFromTable (DbTable _ dbFields _) = dbFields

primaryKeyToSql :: [String] -> String
primaryKeyToSql keys = "PRIMARY KEY(" ++ intercalate "," keys ++ ")"

fieldTypeToSql :: DbFieldType -> String
fieldTypeToSql (Varchar s) = "VARCHAR(" ++ show s ++ ")"
fieldTypeToSql (DbInt) = "INT"

isNotNullToSql :: Bool -> String
isNotNullToSql isNotNull = if(isNotNull) then "NOT NULL" else "NULL"

fieldToSql :: DbField -> String
fieldToSql (DbField name fieldType isNotNull) = intercalate " " [name, fieldTypeToSql fieldType, isNotNullToSql isNotNull]

fieldsToSql :: [DbField] -> String
fieldsToSql dbFields = intercalate "," $ map fieldToSql dbFields

extractNameFromDbField :: DbField -> String
extractNameFromDbField (DbField name _ _) = name

dbFieldsToSqlNames :: [DbField] -> String
dbFieldsToSqlNames dbFields = intercalate "," $ map (extractNameFromDbField) dbFields

extractParamNameFromDbField :: DbField -> String
extractParamNameFromDbField dbField = "@" ++ extractNameFromDbField dbField

dbFieldsToSqlParams :: [DbField] -> String
dbFieldsToSqlParams dbFields = intercalate "," $ map (extractParamNameFromDbField) dbFields

fromSql :: String -> String
fromSql tableName = "From " ++ tableName

whereSql :: SearchDbFields -> String
whereSql searchFields = "Where " ++ searchFieldsToSql
    where
        searchFieldsToSql = intercalate "," $ map searchFieldToSql searchFields
        searchFieldToSql searchField@(DbField _ _ True) = (extractNameFromDbField searchField) ++ "=" ++ (extractParamNameFromDbField searchField)
        searchFieldToSql searchField@(DbField _ _ False) = "(case WHEN " ++ paramName ++ " IS NULL THEN (" ++ fieldName ++ " is NULL) else (" ++ fieldName ++ " = " ++ paramName ++ ") END)"
            where
                paramName = extractParamNameFromDbField searchField
                fieldName = extractNameFromDbField searchField

-- (case WHEN @P1 IS NULL THEN (DbColumn is NULL) else (DbColumn = @P1) END)
tableBody :: [DbField] -> [String] -> String
tableBody dbFields [] = fieldsToSql dbFields
tableBody dbFields keys = fieldsToSql dbFields ++ "," ++ primaryKeyToSql keys 

tableToSqlCreateTable :: DbTable -> String
tableToSqlCreateTable (DbTable tableName dbFields primaryKeys) =  "CREATE TABLE " ++ tableName ++ "(" ++ tableBody dbFields primaryKeys ++ ") WITHOUT ROWID"

tableToSqlInsertOrUpdate :: DbTable -> String
tableToSqlInsertOrUpdate (DbTable tableName dbFields _) = "insert or replace into " ++ tableName ++" (" ++ dbFieldsToSqlNames dbFields ++ ") values (" ++ dbFieldsToSqlParams dbFields ++ ")"

tableToSqlReader :: DbTable -> SearchDbFields -> String
tableToSqlReader (DbTable tableName dbFields _) searchFields = intercalate " " ["SELECT", dbFieldsToSqlNames dbFields, fromSql tableName, whereSql searchFields]

