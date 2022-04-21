module MiniJuvix.Syntax.CJuvix.Language where

import MiniJuvix.Prelude hiding (Enum)

newtype CCode = CCode {_ccodeExternal :: [External]}

data External
  = ExternalDecl Declaration
  | ExternalFunc Function

--------------------------------------------------------------------------------
-- Declaration
--------------------------------------------------------------------------------

data Declaration = Declaration
  { _declType :: Maybe DeclType,
    _declIsPtr :: Bool,
    _declName :: Text,
    _declInitializer :: Maybe Initializer
  }

data Initializer
  = ExprInitializer Expression
  | DesignatorInitializer [DesigInit]

data DesigInit = DesigInit
  { _desigDesignator :: Text,
    _desigInitializer :: Initializer
  }

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

data Function = Function
  { _funcReturnType :: DeclType,
    _funcIsPtr :: Bool,
    _funcQualifier :: Qualifier,
    _funcName :: Text,
    _funcArgs :: [Declaration],
    _funcBody :: [BodyItem]
  }

data BodyItem
  = BodyStatement Statement
  | BodyDecl Declaration

data Qualifier
  = StaticInline
  | None
  deriving stock (Eq)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data DeclType
  = DeclTypeDefType Text
  | DeclStructUnion StructUnion
  | DeclTypeDef DeclType
  | DeclEnum Enum
  | BoolType

data StructUnion = StructUnion
  { _structUnionTag :: StructUnionTag,
    _structUnionName :: Maybe Text,
    _structMembers :: Maybe [Declaration]
  }

data StructUnionTag
  = StructTag
  | UnionTag

data Enum = Enum
  { _enumName :: Maybe Text,
    _enumMembers :: Maybe [Text]
  }

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data Expression
  = ExpressionAssign Assign
  | ExpressionCast Cast
  | ExpressionCall Call
  | ExpressionLiteral Literal
  | ExpressionVar Text
  | ExpressionBinary Binary
  | ExpressionUnary Unary
  | ExpressionMember MemberAccess

data Assign = Assign
  { _assignLeft :: Expression,
    _assignRight :: Expression
  }

data Cast = Cast
  { _castDecl :: Declaration,
    _castExpression :: Expression
  }

data Call = Call
  { _callCallee :: Expression,
    _callArgs :: [Expression]
  }

data Literal
  = LiteralInt Integer
  | LiteralChar Char
  | LiteralString Text

data BinaryOp
  = Eq
  | Neq
  | And
  | Or

data Binary = Binary
  { _binaryOp :: BinaryOp,
    _binaryLeft :: Expression,
    _binaryRight :: Expression
  }

data UnaryOp
  = Address
  | Indirection
  | Negation

data Unary = Unary
  { _unaryOp :: UnaryOp,
    _unarySubject :: Expression
  }

data MemberAccessOp
  = Object
  | Pointer
  deriving stock (Eq)

data MemberAccess = MemberAccess
  { _memberSubject :: Expression,
    _memberField :: Text,
    _memberOp :: MemberAccessOp
  }

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Statement
  = StatementReturn (Maybe Expression)
  | StatementIf If
  | StatementExpr Expression

data If = If
  { _ifCondition :: Expression,
    _ifThen :: Statement,
    _ifElse :: Maybe Statement
  }
