module MiniJuvix.Translation.MonoJuvixToC where

import Data.Text qualified as T
import MiniJuvix.Prelude
import MiniJuvix.Syntax.CJuvix.Language
import MiniJuvix.Syntax.CJuvix.Serialization
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Micro
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Syntax.NameId (unNameId)

type Err = Text

unsupported :: Err -> a
unsupported msg = error (msg <> "Mono to C: not yet supported")

type CTypeName = Text

asStruct :: Text -> Text
asStruct n = n <> "_s"

asTypeDef :: Text -> Text
asTypeDef n = n <> "_t"

asTag :: Text -> Text
asTag n = n <> "_tag"

asNew :: Text -> Text
asNew n = n <> "_new"

mkName :: Mono.Name -> Text
mkName name = nameText <> "_" <> nameId
  where
    nameText :: Text
    nameText = T.toLower (name ^. Mono.nameText)
    nameId :: Text
    nameId = show (name ^. Mono.nameId . unNameId)

mkInductiveName :: Mono.InductiveDef -> Text
mkInductiveName i = mkName (i ^. Mono.inductiveName)

mkCtorName :: Mono.InductiveDef -> Mono.InductiveConstructorDef -> Text
mkCtorName i ctor =
  mkInductiveName i <> "_" <> mkName (ctor ^. Mono.constructorName)

mkInductiveConstructorNames :: Mono.InductiveDef -> [Text]
mkInductiveConstructorNames i = mkCtorName i <$> i ^. Mono.inductiveConstructors

goInductiveDef :: Mono.InductiveDef -> [CCode]
goInductiveDef i =
  [ ExternalDecl structTypeDef,
    ExternalDecl tagsType
  ]
    <> (i ^. Mono.inductiveConstructors >>= goInductiveConstructorDef i)
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
          _funcName = baseName <> "_is_" <> ctorName,
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
        funcArg = "v"

    asFunction :: Text -> Function
    asFunction ctorName =
      Function
        { _funcReturnType = DeclTypeDefType (asTypeDef ctorName),
          _funcIsPtr = False,
          _funcQualifier = StaticInline,
          _funcName = baseName <> "_as_" <> ctorName,
          _funcArgs = [ptrType (DeclTypeDefType (asTypeDef baseName)) funcArg],
          _funcBody =
            [ returnStatement
                (memberAccess Object (memberAccess Pointer (ExpressionVar funcArg) "data") ctorName)
            ]
        }
      where
        funcArg :: Text
        funcArg = "v"

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
    baseName = mkCtorName i ctor

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
          _funcName = inductiveName <> "_new_" <> baseName,
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

inductiveCtorArgs :: Mono.InductiveConstructorDef -> [Declaration]
inductiveCtorArgs ctor = zipWith goType argLabels ctorParams
  where
    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Micro.constructorParameters

    argLabels :: [Text]
    argLabels = (\l -> "v" <> show l) <$> [0 :: Integer ..]

goInductiveConstructorDef ::
  Mono.InductiveDef ->
  Mono.InductiveConstructorDef ->
  [CCode]
goInductiveConstructorDef i ctor =
  [ExternalDecl ctorDecl]
  where
    ctorDecl :: Declaration
    ctorDecl = if null ctorParams then ctorBool else ctorStruct

    baseName :: Text
    baseName = mkCtorName i ctor

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

goType :: Text -> Mono.Type -> Declaration
goType n = \case
  Mono.TypeIden ti ->
    Declaration
      { _declType = DeclTypeDefType (asTypeDef (mkName (getMonoName ti))),
        _declIsPtr = True,
        _declName = Just n,
        _declInitializer = Nothing
      }
  Mono.TypeFunction {} -> unsupported "TypeFunction"
  Mono.TypeUniverse {} -> unsupported "TypeUniverse"
  Mono.TypeAny {} -> unsupported "TypeAny"
  where
    getMonoName :: Mono.TypeIden -> Mono.Name
    getMonoName = \case
      Mono.TypeIdenInductive mn -> mn
      Mono.TypeIdenAxiom mn -> mn

mallocSizeOf :: Text -> Expression
mallocSizeOf typeName =
  functionCall "malloc" [functionCall "sizeof" [ExpressionVar typeName]]

exInductive :: Mono.InductiveDef
exInductive =
  Mono.InductiveDef
    { _inductiveName =
        Mono.Name
          { _nameText = "Indu",
            _nameId = Mono.NameId 11,
            _nameKind = Mono.KNameInductive,
            _nameDefined = Nothing,
            _nameLoc = Nothing
          },
      _inductiveConstructors = [exConstructor "zero" 111 [], exConstructor "cons" 12 ["arg1", "arg2"]]
    }

exConstructor :: Text -> Integer -> [Text] -> Mono.InductiveConstructorDef
exConstructor n i args =
  Mono.InductiveConstructorDef
    { _constructorName =
        Mono.Name
          { _nameText = n,
            _nameId = Mono.NameId (fromInteger i),
            _nameKind = Mono.KNameConstructor,
            _nameDefined = Nothing,
            _nameLoc = Nothing
          },
      _constructorParameters = map (Mono.TypeIden . Mono.TypeIdenInductive . typeName) args
    }
  where
    typeName :: Text -> Mono.Name
    typeName tn =
      Mono.Name
        { _nameText = tn,
          _nameId = Mono.NameId 101,
          _nameKind = Mono.KNameInductive,
          _nameDefined = Nothing,
          _nameLoc = Nothing
        }

exCode :: CCodeUnit
exCode = CCodeUnit [] (goInductiveDef exInductive)
