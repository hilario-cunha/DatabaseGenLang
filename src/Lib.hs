module Lib
    ( someFunc
    ) where

import Language.CSharp.Syntax
import Language.CSharp.Pretty
import Gen
import CSharpGen
import Db
import DbCSharpGen

someFunc :: IO ()
someFunc = do
    generateTableClasses createTableWithFunctionalityNameForReasons
    generateTableClasses createTableWithFunctionalityNameForZones
    generateTableClasses createTableWithFunctionalityNameForAlertColors
    generateTableClasses createTableWithFunctionalityNameForAlerts
    generateTableClasses createTableWithFunctionalityNameForWithdrawModalControl

--  @"CREATE TABLE ra_WithdrawModalControl(
--                         FeatureId VARCHAR(50) NOT NULL,
--                         Code VARCHAR(50) NOT NULL,
--                         Visible BIT NOT NULL
--                     )"
createTableWithFunctionalityNameForWithdrawModalControl :: TableWithFunctionalityName
createTableWithFunctionalityNameForWithdrawModalControl = TableWithFunctionalityName functionalityName zonesTable searchDbFields
    where 
        functionalityName = "WithdrawModalControl"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        featureIdName = "FeatureId"
        featureIdbField = DbField featureIdName (Varchar 50) True
        codeName = "Code"
        zonesTable = DbTable 
            tableName
            [ featureIdbField
            , DbField codeName (Varchar 50) True
            , DbField "Visible" Bit True
            ]
            [ featureIdName, codeName]
        searchDbFields = [featureIdbField]

createTableWithFunctionalityNameForAlerts :: TableWithFunctionalityName
createTableWithFunctionalityNameForAlerts = TableWithFunctionalityName functionalityName zonesTable searchDbFields
    where 
        functionalityName = "Alerts"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        itemIdName = "ItemId"
        itemIdDbField = DbField itemIdName (Varchar 250) True
        parentIdName = "ParentId"
        parentIdDbField = DbField parentIdName (Varchar 250) False
        alertIdName = "AlertId"
        zonesTable = DbTable 
            tableName
            [ itemIdDbField
            , parentIdDbField
            , DbField alertIdName (Varchar 250) True
            , DbField "Level" DbInt True
            , DbField "Name" (Varchar 250) False
            ]
            [ itemIdName, parentIdName, alertIdName]
        searchDbFields = [itemIdDbField, parentIdDbField]

createTableWithFunctionalityNameForAlertColors :: TableWithFunctionalityName
createTableWithFunctionalityNameForAlertColors = TableWithFunctionalityName functionalityName zonesTable searchDbFields
    where 
        functionalityName = "AlertColors"
        tablePrefix = "ra_"
        tableName = tablePrefix ++ functionalityName
        featureIdDbField = DbField "FeatureId" (Varchar 50) True
        zonesTable = DbTable 
            tableName
            [ featureIdDbField
            , DbField "RangeBegin" DbInt True
            , DbField "RangeEnd" DbInt True
            , DbField "Red" DbInt True
            , DbField "Green" DbInt True
            , DbField "Blue" DbInt True
            ]
            [ "FeatureId", "RangeBegin", "RangeEnd"]
        searchDbFields = [featureIdDbField]

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
    , createNamespaceWithClassForRow tableWithFunctionalityName
    ]

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

createAndWriteToFile :: Pretty a => FilePath -> a -> IO ()
createAndWriteToFile fileName cu  = writeFile fileName $ prettyPrint cu      
