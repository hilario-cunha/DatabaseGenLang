module CSharpGen where

import Language.CSharp.Syntax
import Data.Char (toLower, toUpper)

mkUsings :: [String] -> [Using]
mkUsings = map mkUsing

mkUsing :: String -> Using
mkUsing u = Using (mkName u) False

mkNamespace :: String -> [Declaration] -> Declaration
mkNamespace ns = NamespaceDeclaration [] (mkName ns)

camelCase :: String -> String
camelCase (head:tail) = toLower head : tail
camelCase [] = []

capitalize :: String -> String
capitalize (head:tail) = toUpper head : tail
capitalize [] = []

mkAssignThisDot :: String -> String -> Expression
mkAssignThisDot l r = mkAssign (MemberAccess $ mkPrimaryMemberAccessThisDot l) (mkSimpleName r)

mkArgument :: Expression -> Argument
mkArgument = Argument Nothing

mkSimpleNameArgument :: String -> Argument
mkSimpleNameArgument n = mkArgument (mkSimpleName n)

mkLiteralStringArgument :: String -> Argument
mkLiteralStringArgument s = mkArgument (mkLiteralString s)

mkLiteralString :: String -> Expression
mkLiteralString s = Literal (StringLit s)

mkFormalParam :: String -> String -> FormalParam
mkFormalParam t n = FormalParam (Nothing) (mkTypeNamed t) (Identifier n) (Nothing)

mkPublicClass :: String -> [MemberDeclaration] -> Declaration
mkPublicClass className cb = TypeDeclaration $ ClassTypeDeclaration [] [Public, Partial] (Identifier className) [] [] [] (ClassBody cb)

mkTypeNamed :: String -> Type
mkTypeNamed t = mkTypeNamedWithTypeArguments t []

mkTypeNamedWithTypeArguments :: String -> [TypeArgument] -> Type
mkTypeNamedWithTypeArguments t args = (TypeNamed (TypeName (mkName t) args))

mkName :: String -> Name
mkName n = (Name [Identifier n])

mkPrimaryMemberAccess :: Expression -> String -> MemberAccess
mkPrimaryMemberAccess obj p = mkPrimaryMemberAccessWithTypeArguments obj p []

mkPrimaryMemberAccessWithTypeArguments :: Expression -> String -> [TypeArgument] -> MemberAccess
mkPrimaryMemberAccessWithTypeArguments obj p typeArguments = PrimaryMemberAccess obj (Identifier p) typeArguments

mkPrimaryMemberAccessThisDot :: String -> MemberAccess
mkPrimaryMemberAccessThisDot p = PrimaryMemberAccess This (Identifier p) []


mkAssign :: Expression -> Expression -> Expression
mkAssign l r = Assign l OpAssign r

mkAssignStatement :: String -> String -> Statement
mkAssignStatement l r = ExpressionStatement $ mkAssign (SimpleName (Identifier l) []) (SimpleName (Identifier r) [])

mkNewArgument :: String -> [Argument] -> Argument
mkNewArgument cn args = Argument Nothing (mkNew cn args)

mkNew :: String -> [Argument] -> Expression
mkNew cn args = ObjectCreationExpression (mkTypeNamed cn) args Nothing

mkSimpleName :: String -> Expression
mkSimpleName n = SimpleName (Identifier n) []

ifThenBinaryOp :: BinaryOperator -> Expression -> Expression -> Statement -> Statement
ifThenBinaryOp ifOp if1 if2 thenOp = ifThen (BinaryOperator ifOp if1 if2) thenOp

ifThen :: Expression -> Statement -> Statement
ifThen ifOp thenOp = IfThenElse
                ifOp 
                thenOp
                Nothing

mkReturn :: Expression -> Statement
mkReturn exp = Return $ Just exp

mkAutoGetAccessorDeclaration :: [Modifier] -> Maybe AccessorDeclaration
mkAutoGetAccessorDeclaration modifiers = Just $ GetAccessorDeclaration [] modifiers Nothing

mkAutoSetAccessorDeclaration :: [Modifier] -> Maybe AccessorDeclaration
mkAutoSetAccessorDeclaration modifiers = Just $ SetAccessorDeclaration [] modifiers Nothing

mkPropertyBody :: Maybe AccessorDeclaration -> Maybe AccessorDeclaration -> PropertyBody
mkPropertyBody getAccessorDeclaration setAccessorDeclaration = PropertyBody getAccessorDeclaration setAccessorDeclaration Nothing

mkPropertyMemberDeclaration :: [Modifier] -> Type -> Name -> PropertyBody -> MemberDeclaration
mkPropertyMemberDeclaration modifiers _type name body = PropertyMemberDeclaration [] modifiers _type name body

mkAutoPropertyBodyPrivateSet :: PropertyBody
mkAutoPropertyBodyPrivateSet = mkPropertyBody (mkAutoGetAccessorDeclaration []) (mkAutoSetAccessorDeclaration [Private])

mkPropertyAutoPublicGet :: String -> String -> MemberDeclaration
mkPropertyAutoPublicGet t n = mkPropertyMemberDeclaration [Public] (mkTypeNamed t) (mkName n) (mkAutoPropertyBodyPrivateSet)

mkFormalParams :: [FormalParam] -> FormalParams
mkFormalParams params = FormalParams params (Nothing)

mkConstructorMemberDeclaration :: [Modifier] -> String -> [FormalParam] -> [Statement] -> MemberDeclaration
mkConstructorMemberDeclaration modifiers name formalParams body = ConstructorMemberDeclaration [] modifiers (Identifier name) (mkFormalParams formalParams) Nothing (ConstructorStatementBody body)

mkMethodMemberDeclaration :: [Modifier] -> Type -> String -> [FormalParam] -> [Statement] -> MemberDeclaration
mkMethodMemberDeclaration modifiers returnType name formalParams bodyStatements = mkMethodMemberDeclarationWithTypeParameters modifiers (Just returnType) name [] formalParams [] bodyStatements

mkMethodMemberDeclarationWithVoidReturn :: [Modifier] -> String -> [FormalParam] -> [Statement] -> MemberDeclaration
mkMethodMemberDeclarationWithVoidReturn modifiers name formalParams bodyStatements = mkMethodMemberDeclarationWithTypeParameters modifiers Nothing name [] formalParams [] bodyStatements

mkMethodMemberDeclarationWithTypeParameters :: [Modifier] -> Maybe Type -> String -> [TypeParameter] -> [FormalParam] -> [TypeParameterConstraintClause] -> [Statement] -> MemberDeclaration
mkMethodMemberDeclarationWithTypeParameters modifiers mType name typeParameters formalParams typeParameterConstraintClauses bodyStatements = MethodMemberDeclaration [] modifiers mType (mkName name) typeParameters (mkFormalParams formalParams) typeParameterConstraintClauses (MethodStatementBody bodyStatements)

mkMethodMemberDeclarationPublicStatic :: Type -> String -> [FormalParam] -> [Statement] -> MemberDeclaration
mkMethodMemberDeclarationPublicStatic = mkMethodMemberDeclaration [Public, Static]  

mkInvocationSimpleName :: String -> [Argument] -> Expression
mkInvocationSimpleName n = Invocation (mkSimpleName n)

mkThrow :: Expression -> Statement
mkThrow = Throw . Just

mkNewArgumentNullException :: String -> String -> Expression
mkNewArgumentNullException n t = mkNew "System.ArgumentNullException" [mkLiteralStringArgument n, mkLiteralStringArgument $ "Field "++ n ++" with type " ++ t ++ " can not be null"]

mkThrowArgumentNullException :: String -> String -> Statement
mkThrowArgumentNullException n t = mkThrow $ mkNewArgumentNullException n t

ifNullThen :: String -> Statement -> Statement
ifNullThen n = ifThenBinaryOp BinaryEquals (mkSimpleName n) (Literal NullLit)

mkChoiceT :: String -> String -> String
mkChoiceT c1T c2T = "Choice<" ++ c1T ++ ", " ++ c2T ++ ">"

mkChoice2Of2 :: String -> [Argument] -> Expression
mkChoice2Of2 choiceT = mkInvocationSimpleName $ choiceT ++ ".Choice2Of2"

mkChoice2Of2Return :: String -> [Argument] -> Statement
mkChoice2Of2Return choiceT args = mkReturn $ mkChoice2Of2 choiceT args

mkChoice1Of2 :: String -> [Argument] -> Expression
mkChoice1Of2 choiceT = mkInvocationSimpleName $ choiceT ++ ".Choice1Of2"

mkChoice1Of2Return :: String -> [Argument] -> Statement
mkChoice1Of2Return choiceT args = mkReturn $ mkChoice1Of2 choiceT args

mkTypeNamedTypeArgument :: String -> TypeArgument
mkTypeNamedTypeArgument t = TypeArgument (mkTypeNamed t)

mkTypeArrayTypeArgument :: String -> TypeArgument
mkTypeArrayTypeArgument t = (TypeArgument (mkTypeArray t))

mkTypeArray :: String -> Type
mkTypeArray t = TypeArray (ArrayType (mkTypeNamed t) [RankSpecifier 0])

mkField :: String -> String -> MemberDeclaration
mkField t n = FieldMemberDeclaration [] [] (mkTypeNamed t) [mkVariableDeclarator n]

mkVariableDeclarator :: String -> VariableDeclarator
mkVariableDeclarator n = VariableDeclarator (Identifier n) (Nothing)

mkVariableDeclaratorWithInitializerExpression :: String -> Expression -> VariableDeclarator
mkVariableDeclaratorWithInitializerExpression n exp = VariableDeclarator (Identifier n) (Just (VariableInitializerExpression exp))

mkLocalVarDeclarationVar :: [VariableDeclarator] -> LocalVarDeclaration
mkLocalVarDeclarationVar = LocalVarDeclaration Var 
        
mkAndInitLocalVar :: String -> Expression -> Statement
mkAndInitLocalVar varName exp = Declaration (mkLocalVarDeclarationVar [mkVariableDeclaratorWithInitializerExpression varName exp])

