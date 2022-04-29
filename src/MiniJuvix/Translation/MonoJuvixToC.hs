module MiniJuvix.Translation.MonoJuvixToC where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.CJuvix.Language
import MiniJuvix.Syntax.CJuvix.Serialization
import MiniJuvix.Syntax.Concrete.Language qualified as C
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Micro
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult qualified as Mono

newtype MiniCResult = MiniCResult
  { _resultCCode :: Text
  }

makeLenses ''MiniCResult

entryMiniC ::
  Mono.MonoJuvixResult ->
  Sem r MiniCResult
entryMiniC i = return (MiniCResult (serialize cunitResult))
  where
    cunitResult :: CCodeUnit
    cunitResult =
      CCodeUnit
        { _ccodeCode = cheader <> (toList (i ^. Mono.resultModules) >>= goModule)
        }
    cheader =
      map
        ExternalMacro
        [ CppIncludeSystem "stdlib.h",
          CppIncludeSystem "stdbool.h"
        ]

type Err = Text

unsupported :: Err -> a
unsupported msg = error (msg <> " Mono to C: not yet supported")

goModule :: Mono.Module -> [CCode]
goModule Mono.Module {..} = goModuleBody _moduleBody

goModuleBody ::
  Mono.ModuleBody ->
  [CCode]
goModuleBody Mono.ModuleBody {..} = _moduleStatements >>= goStatement

goStatement :: Mono.Statement -> [CCode]
goStatement = \case
  Mono.StatementInductive d -> goInductiveDef d
  Mono.StatementFunction d -> goFunctionDef d
  Mono.StatementForeign d -> goForeign d
  Mono.StatementAxiom d -> goAxiomDef d

type CTypeName = Text

asStruct :: Text -> Text
asStruct n = n <> "_s"

asTypeDef :: Text -> Text
asTypeDef n = n <> "_t"

asTag :: Text -> Text
asTag n = n <> "_tag"

asNew :: Text -> Text
asNew n = "new_" <> n

asCast :: Text -> Text
asCast n = "as_" <> n

asIs :: Text -> Text
asIs n = "is_" <> n

mkName :: Mono.Name -> Text
mkName name = nameText
  where
    nameText :: Text
    nameText = T.toLower (name ^. Mono.nameText)

goFunctionDef :: Mono.FunctionDef -> [CCode]
goFunctionDef Mono.FunctionDef {..} =
  [ ExternalFunc
      ( Function
          { _funcReturnType = _typeDeclType funReturnType,
            _funcIsPtr = _typeIsPtr funReturnType,
            _funcQualifier = None,
            _funcName = mkName _funDefName,
            _funcArgs = namedArgs "fa" funArgTypes,
            _funcBody = maybeToList (fmap BodyStatement (mkBody (goFunctionClause <$> toList _funDefClauses)))
          }
      )
  ]
  where
    mkBody :: [(Maybe Expression, Statement)] -> Maybe Statement
    mkBody = foldr mkIf Nothing
    mkIf :: (Maybe Expression, Statement) -> Maybe Statement -> Maybe Statement
    mkIf (mcondition, thenBranch) elseBranch = case mcondition of
      Nothing -> Just thenBranch
      Just condition ->
        Just
          ( StatementIf
              ( If
                  { _ifCondition = condition,
                    _ifThen = thenBranch,
                    _ifElse = elseBranch
                  }
              )
          )
    funArgTypes :: [CDeclType]
    funArgTypes = fst funType
    funReturnType :: CDeclType
    funReturnType = snd funType
    funType :: ([CDeclType], CDeclType)
    funType = unfoldFunType _funDefType
    unfoldFunType :: Mono.Type -> ([CDeclType], CDeclType)
    unfoldFunType = \case
      Mono.TypeFunction (Mono.Function l r) -> first (goType l :) (unfoldFunType r)
      t -> ([], goType t)

goFunctionClause :: Mono.FunctionClause -> (Maybe Expression, Statement)
goFunctionClause Mono.FunctionClause {..} = (clauseCondition, returnStmt)
  where
    varToArg :: HashMap Text Expression
    varToArg = HashMap.fromList patternVars
    conditions :: [Expression]
    conditions = do
      (p, n) <- zip _clausePatterns [0 :: Integer ..]
      let arg = ExpressionVar ("fa" <> show n)
      case p of
        Mono.PatternConstructorApp (Mono.ConstructorApp {..}) ->
          [functionCall (ExpressionVar (asIs (mkName _constrAppConstructor))) [arg]]
        Mono.PatternVariable {} -> []
        Mono.PatternWildcard {} -> []

    clauseCondition :: Maybe Expression
    clauseCondition = fmap (foldr1 f) (nonEmpty conditions)
      where
        f :: Expression -> Expression -> Expression
        f e1 e2 =
          ExpressionBinary
            ( Binary
                { _binaryOp = And,
                  _binaryLeft = e1,
                  _binaryRight = e2
                }
            )

    patternVars :: [(Text, Expression)]
    patternVars = do
      (p, n) <- zip _clausePatterns [0 :: Integer ..]
      let arg = ExpressionVar ("fa" <> show n)
      case p of
        Mono.PatternVariable v -> [(v ^. Mono.nameText, arg)]
        Mono.PatternConstructorApp (Mono.ConstructorApp {..}) ->
          goConstructorApp arg _constrAppConstructor _constrAppParameters
        Mono.PatternWildcard {} -> []
    returnStmt :: Statement
    returnStmt = StatementReturn (Just (goExpression False varToArg _clauseBody))

goConstructorApp :: Expression -> Mono.Name -> [Mono.Pattern] -> [(Text, Expression)]
goConstructorApp arg n ps = do
  (p, idx) <- zip ps [0 :: Integer ..]
  let field = "ca" <> show idx
  case p of
    Mono.PatternVariable v -> [(v ^. Mono.nameText, memberAccess Object asConstructor field)]
    Mono.PatternConstructorApp {} -> unsupported "PatternConstructorApp in pattern"
    Mono.PatternWildcard {} -> []
  where
    asConstructor :: Expression
    asConstructor = functionCall (ExpressionVar (asCast (mkName n))) [arg]

goExpression :: Bool -> HashMap Text Expression -> Mono.Expression -> Expression
goExpression fromApplication varToArg = \case
  Mono.ExpressionIden i -> goIden fromApplication varToArg i
  Mono.ExpressionApplication a -> goApplication varToArg a
  Mono.ExpressionLiteral l -> goLiteral l
  Mono.ExpressionTyped Mono.TypedExpression {..} -> goExpression fromApplication varToArg _typedExpression

goIden :: Bool -> HashMap Text Expression -> Mono.Iden -> Expression
goIden fromApplication varToArg = \case
  Mono.IdenFunction n -> if fromApplication then e else functionCall e []
    where
      e :: Expression
      e = ExpressionVar (mkName n)
  Mono.IdenConstructor n -> if fromApplication then newCtor else functionCall newCtor []
    where
      newCtor :: Expression
      newCtor = ExpressionVar (asNew (mkName n))
  Mono.IdenVar n -> HashMap.lookupDefault impossible (n ^. Mono.nameText) varToArg
  Mono.IdenAxiom n -> ExpressionVar (mkName n)

goApplication :: HashMap Text Expression -> Mono.Application -> Expression
goApplication varToArg a = functionCall (fst f) (reverse (snd f))
  where
    f :: (Expression, [Expression])
    f = unfoldApp a
    unfoldApp :: Mono.Application -> (Expression, [Expression])
    unfoldApp Mono.Application {..} = case _appLeft of
      Mono.ExpressionApplication x -> second (goExpression False varToArg _appRight :) (unfoldApp x)
      _ -> (goExpression True varToArg _appLeft, [goExpression False varToArg _appRight])

goLiteral :: C.LiteralLoc -> Expression
goLiteral C.LiteralLoc {..} = case _literalLocLiteral of
  C.LitString s -> ExpressionLiteral (LiteralString s)
  C.LitInteger i -> ExpressionLiteral (LiteralInt i)

goAxiomDef :: Mono.AxiomDef -> [CCode]
goAxiomDef a =
  case firstJust getCode backends of
    Nothing -> error ("c does not support this primitive:" <> show (a ^. Mono.axiomName))
    Just _defineBody ->
      [ExternalMacro (CppDefine (Define {..}))]
  where
    _defineName :: Text
    _defineName = mkName (a ^. Mono.axiomName)
    backends :: [BackendItem]
    backends = a ^. Mono.axiomBackendItems
    getCode :: BackendItem -> Maybe Text
    getCode b =
      guard (BackendC == b ^. backendItemBackend)
        $> b ^. backendItemCode

goForeign :: ForeignBlock -> [CCode]
goForeign b = case b ^. foreignBackend of
  BackendC -> [Verbatim (b ^. foreignCode)]
  _ -> []

mkInductiveName :: Mono.InductiveDef -> Text
mkInductiveName i = mkName (i ^. Mono.inductiveName)

mkInductiveConstructorNames :: Mono.InductiveDef -> [Text]
mkInductiveConstructorNames i = mkName . view Mono.constructorName <$> i ^. Mono.inductiveConstructors

goInductiveDef :: Mono.InductiveDef -> [CCode]
goInductiveDef i =
  [ ExternalDecl structTypeDef,
    ExternalDecl tagsType
  ]
    <> (i ^. Mono.inductiveConstructors >>= goInductiveConstructorDef)
    <> [ExternalDecl inductiveDecl]
    <> (i ^. Mono.inductiveConstructors >>= goInductiveConstructorNew i)
    <> (ExternalFunc . isFunction <$> constructorNames)
    <> (ExternalFunc . asFunction <$> constructorNames)
  where
    baseName :: Text
    baseName = mkName (i ^. Mono.inductiveName)

    constructorNames :: [Text]
    constructorNames = mkInductiveConstructorNames i

    structTypeDef :: Declaration
    structTypeDef =
      typeDefWrap
        (asTypeDef baseName)
        ( DeclStructUnion
            ( StructUnion
                { _structUnionTag = StructTag,
                  _structUnionName = Just (asStruct baseName),
                  _structMembers = Nothing
                }
            )
        )

    tagsType :: Declaration
    tagsType =
      typeDefWrap
        (asTag baseName)
        ( DeclEnum
            ( Enum
                { _enumName = Nothing,
                  _enumMembers = Just (asTag <$> constructorNames)
                }
            )
        )

    inductiveDecl :: Declaration
    inductiveDecl =
      Declaration
        { _declType = inductiveStruct,
          _declIsPtr = False,
          _declName = Nothing,
          _declInitializer = Nothing
        }

    inductiveStruct :: DeclType
    inductiveStruct =
      DeclStructUnion
        ( StructUnion
            { _structUnionTag = StructTag,
              _structUnionName = Just (asStruct baseName),
              _structMembers =
                Just
                  [ typeDefType (asTag baseName) "tag",
                    Declaration
                      { _declType = unionMembers,
                        _declIsPtr = False,
                        _declName = Just "data",
                        _declInitializer = Nothing
                      }
                  ]
            }
        )

    unionMembers :: DeclType
    unionMembers =
      DeclStructUnion
        ( StructUnion
            { _structUnionTag = UnionTag,
              _structUnionName = Nothing,
              _structMembers = Just (map (\ctorName -> typeDefType (asTypeDef ctorName) ctorName) constructorNames)
            }
        )

    isFunction :: Text -> Function
    isFunction ctorName =
      Function
        { _funcReturnType = BoolType,
          _funcIsPtr = False,
          _funcQualifier = StaticInline,
          _funcName = asIs ctorName,
          _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg],
          _funcBody =
            [ returnStatement
                ( equals
                    (memberAccess Pointer (ExpressionVar funcArg) "tag")
                    (ExpressionVar (asTag ctorName))
                )
            ]
        }
      where
        funcArg :: Text
        funcArg = "a"

    asFunction :: Text -> Function
    asFunction ctorName =
      Function
        { _funcReturnType = DeclTypeDefType (asTypeDef ctorName),
          _funcIsPtr = False,
          _funcQualifier = StaticInline,
          _funcName = asCast ctorName,
          _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg],
          _funcBody =
            [ returnStatement
                (memberAccess Object (memberAccess Pointer (ExpressionVar funcArg) "data") ctorName)
            ]
        }
      where
        funcArg :: Text
        funcArg = "a"

goInductiveConstructorNew ::
  Mono.InductiveDef ->
  Mono.InductiveConstructorDef ->
  [CCode]
goInductiveConstructorNew i ctor =
  [ExternalFunc ctorNewFun]
  where
    ctorNewFun :: Function
    ctorNewFun = if null ctorParams then ctorNewNullary else ctorNewNary

    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    inductiveName :: Text
    inductiveName = mkInductiveName i

    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Micro.constructorParameters

    ctorNewNullary :: Function
    ctorNewNullary =
      commonFunctionDeclr
        []
        [ BodyDecl allocInductive,
          BodyDecl (commonInitDecl (dataInit "true")),
          BodyStatement assignPtr,
          returnStatement (ExpressionVar tmpPtrName)
        ]

    ctorNewNary :: Function
    ctorNewNary =
      commonFunctionDeclr
        ctorArgs
        [ BodyDecl allocInductive,
          BodyDecl ctorStructInit,
          BodyDecl (commonInitDecl (dataInit tmpCtorStructName)),
          BodyStatement assignPtr,
          returnStatement (ExpressionVar tmpPtrName)
        ]
      where
        ctorArgs :: [Declaration]
        ctorArgs = inductiveCtorArgs ctor

        ctorInit :: [DesigInit]
        -- TODO: _declName is never Nothing by construction, fix the types
        ctorInit = map (f . fromJust . _declName) ctorArgs

        f :: Text -> DesigInit
        f fieldName =
          DesigInit
            { _desigDesignator = fieldName,
              _desigInitializer = ExprInitializer (ExpressionVar fieldName)
            }

        ctorStructInit :: Declaration
        ctorStructInit =
          Declaration
            { _declType = DeclTypeDefType (asTypeDef baseName),
              _declIsPtr = False,
              _declName = Just tmpCtorStructName,
              _declInitializer = Just (DesignatorInitializer ctorInit)
            }

    commonFunctionDeclr :: [Declaration] -> [BodyItem] -> Function
    commonFunctionDeclr args body =
      Function
        { _funcReturnType = DeclTypeDefType (asTypeDef inductiveName),
          _funcIsPtr = True,
          _funcQualifier = StaticInline,
          _funcName = asNew baseName,
          _funcArgs = args,
          _funcBody = body
        }

    commonInitDecl :: Initializer -> Declaration
    commonInitDecl di =
      ( Declaration
          { _declType = DeclTypeDefType (asTypeDef inductiveName),
            _declIsPtr = False,
            _declName = Just tmpStructName,
            _declInitializer =
              Just
                ( DesignatorInitializer
                    [ DesigInit
                        { _desigDesignator = "tag",
                          _desigInitializer = ExprInitializer (ExpressionVar (asTag baseName))
                        },
                      DesigInit
                        { _desigDesignator = "data",
                          _desigInitializer = di
                        }
                    ]
                )
          }
      )

    tmpPtrName :: Text
    tmpPtrName = "n"

    tmpStructName :: Text
    tmpStructName = "m"

    tmpCtorStructName :: Text
    tmpCtorStructName = "s"

    allocInductive :: Declaration
    allocInductive =
      ( Declaration
          { _declType = DeclTypeDefType (asTypeDef inductiveName),
            _declIsPtr = True,
            _declName = Just tmpPtrName,
            _declInitializer = Just (ExprInitializer (mallocSizeOf (asTypeDef inductiveName)))
          }
      )

    dataInit :: Text -> Initializer
    dataInit varName =
      DesignatorInitializer
        [ DesigInit
            { _desigDesignator = baseName,
              _desigInitializer = ExprInitializer (ExpressionVar varName)
            }
        ]

    assignPtr :: Statement
    assignPtr =
      StatementExpr
        ( ExpressionAssign
            ( Assign
                { _assignLeft =
                    ExpressionUnary
                      ( Unary
                          { _unaryOp = Indirection,
                            _unarySubject = ExpressionVar tmpPtrName
                          }
                      ),
                  _assignRight = ExpressionVar tmpStructName
                }
            )
        )

namedArgs :: Text -> [CDeclType] -> [Declaration]
namedArgs prefix = zipWith goTypeDecl argLabels
  where
    argLabels :: [Text]
    argLabels = (\l -> prefix <> show l) <$> [0 :: Integer ..]

inductiveCtorArgs :: Mono.InductiveConstructorDef -> [Declaration]
inductiveCtorArgs ctor = namedArgs "ca" (goType <$> ctorParams)
  where
    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Micro.constructorParameters

goInductiveConstructorDef ::
  Mono.InductiveConstructorDef ->
  [CCode]
goInductiveConstructorDef ctor =
  [ExternalDecl ctorDecl]
  where
    ctorDecl :: Declaration
    ctorDecl = if null ctorParams then ctorBool else ctorStruct

    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Micro.constructorParameters

    ctorBool :: Declaration
    ctorBool = typeDefWrap (asTypeDef baseName) BoolType

    ctorStruct :: Declaration
    ctorStruct = typeDefWrap (asTypeDef baseName) struct

    struct :: DeclType
    struct =
      DeclStructUnion
        ( StructUnion
            { _structUnionTag = StructTag,
              _structUnionName = Just (asStruct baseName),
              _structMembers = Just (inductiveCtorArgs ctor)
            }
        )

data CDeclType = CDeclType
  { _typeDeclType :: DeclType,
    _typeIsPtr :: Bool
  }

goType :: Mono.Type -> CDeclType
goType = \case
  Mono.TypeIden ti -> getMonoType ti
  Mono.TypeFunction {} -> unsupported "TypeFunction"
  Mono.TypeUniverse {} -> unsupported "TypeUniverse"
  Mono.TypeAny {} -> unsupported "TypeAny"
  where
    getMonoType :: Mono.TypeIden -> CDeclType
    getMonoType = \case
      Mono.TypeIdenInductive mn ->
        CDeclType
          { _typeDeclType = DeclTypeDefType (asTypeDef (mkName mn)),
            _typeIsPtr = True
          }
      Mono.TypeIdenAxiom mn ->
        CDeclType
          { _typeDeclType = DeclTypeDefType (mkName mn),
            _typeIsPtr = False
          }

goTypeDecl :: Text -> CDeclType -> Declaration
goTypeDecl n (CDeclType {..}) =
  Declaration
    { _declType = _typeDeclType,
      _declIsPtr = _typeIsPtr,
      _declName = Just n,
      _declInitializer = Nothing
    }

mallocSizeOf :: Text -> Expression
mallocSizeOf typeName =
  functionCall (ExpressionVar "malloc") [functionCall (ExpressionVar "sizeof") [ExpressionVar typeName]]
