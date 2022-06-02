module MiniJuvix.Translation.MonoJuvixToMiniC where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.Concrete.Language qualified as C
import MiniJuvix.Syntax.Concrete.Scoped.InfoTable qualified as S
import MiniJuvix.Syntax.ForeignBlock
import MiniJuvix.Syntax.MiniC.Language
import MiniJuvix.Syntax.MiniC.Serialization
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Syntax.NameId
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono

newtype MiniCResult = MiniCResult
  { _resultCCode :: Text
  }

newtype EmittedClosures = EmittedClosures
  { _emittedNames :: HashSet Mono.NameId
  }

emptyEmittedClosures :: EmittedClosures
emptyEmittedClosures = EmittedClosures HashSet.empty

data CFunType = CFunType
  { _cFunArgTypes :: [CDeclType],
    _cFunReturnType :: CDeclType
  }
  deriving stock (Show, Eq)

makeLenses ''MiniCResult
makeLenses ''EmittedClosures
makeLenses ''CFunType

entryMiniC :: Mono.MonoJuvixResult -> Sem r MiniCResult
entryMiniC i = return (MiniCResult (serialize cunitResult))
  where
    compileInfo :: Mono.CompileInfoTable
    compileInfo = Mono.compileInfoTable i

    cunitResult :: CCodeUnit
    cunitResult =
      CCodeUnit
        { _ccodeCode = cheader <> cmodules
        }

    cheader :: [CCode]
    cheader =
      map
        ExternalMacro
        [ CppIncludeSystem Str.stdbool,
          CppIncludeFile Str.minicRuntime
        ]

    cmodules :: [CCode]
    cmodules = do
      m <- toList (i ^. Mono.resultModules)
      run (runReader (Mono.buildTable m) (runReader compileInfo (goModule m)))

unsupported :: Text -> a
unsupported msg = error (msg <> " Mono to C: not yet supported")

goModule ::
  Members '[Reader Mono.CompileInfoTable, Reader Mono.InfoTable] r =>
  Mono.Module ->
  Sem r [CCode]
goModule Mono.Module {..} = goModuleBody _moduleBody

goModuleBody ::
  Members '[Reader Mono.CompileInfoTable, Reader Mono.InfoTable] r =>
  Mono.ModuleBody ->
  Sem r [CCode]
goModuleBody Mono.ModuleBody {..} =
  evalState emptyEmittedClosures (concatMapM goStatement _moduleStatements)

goStatement ::
  Members '[Reader Mono.CompileInfoTable, Reader Mono.InfoTable, State EmittedClosures] r =>
  Mono.Statement ->
  Sem r [CCode]
goStatement = \case
  Mono.StatementInductive d -> return (goInductiveDef d)
  Mono.StatementFunction d -> goFunctionDef d
  Mono.StatementForeign d -> return (goForeign d)
  Mono.StatementAxiom d -> goAxiom d

asStruct :: Text -> Text
asStruct n = n <> "_s"

asTypeDef :: Text -> Text
asTypeDef n = n <> "_t"

asTag :: Text -> Text
asTag n = n <> "_tag"

asField :: Text -> Text
asField n = n <> "_field"

asNullary :: Text -> Text
asNullary n = n <> "_nullary"

asCast :: Text -> Text
asCast n = "as_" <> n

asIs :: Text -> Text
asIs n = "is_" <> n

asNew :: Text -> Text
asNew n = "new_" <> n

asFun :: Text -> Text
asFun n = n <> "_fun"

asFunArg :: Text -> Text
asFunArg n = "fa" <> n

asCtorArg :: Text -> Text
asCtorArg n = "ca" <> n

mkArgs :: (Text -> Text) -> [Text]
mkArgs f = map (f . show) [0 :: Integer ..]

funArgs :: [Text]
funArgs = mkArgs asFunArg

ctorArgs :: [Text]
ctorArgs = mkArgs asCtorArg

mkName :: Mono.Name -> Text
mkName n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | T.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = T.filter isValidChar (n ^. Mono.nameText)
    isValidChar :: Char -> Bool
    isValidChar c = isLetter c && isAscii c
    adaptFirstLetter :: Text -> Text
    adaptFirstLetter t = case T.uncons t of
      Nothing -> impossible
      Just (h, r) -> T.cons (capitalize h) r
      where
        capitalize :: Char -> Char
        capitalize
          | capital = toUpper
          | otherwise = toLower
        capital = case n ^. Mono.nameKind of
          Mono.KNameConstructor -> True
          Mono.KNameInductive -> True
          Mono.KNameTopModule -> True
          Mono.KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Mono.nameKind of
      Mono.KNameTopModule -> mempty
      Mono.KNameFunction ->
        if n ^. Mono.nameText == Str.main then mempty else idSuffix
      _ -> idSuffix
    idSuffix :: Text
    idSuffix = "_" <> show (n ^. Mono.nameId . unNameId)

initClosures ::
  Members '[Reader Mono.InfoTable] r =>
  Mono.FunctionDef ->
  Sem r [Function]
initClosures = undefined

goFunctionDef ::
  Members '[Reader Mono.InfoTable, State EmittedClosures] r =>
  Mono.FunctionDef ->
  Sem r [CCode]
goFunctionDef Mono.FunctionDef {..} = do
  fc <- mapM (goFunctionClause (fst (unfoldFunType _funDefType))) (toList _funDefClauses)
  let bodySpec = fst <$> fc
  let preDecls :: [Function] = snd =<< fc
  return $
    (ExternalFunc <$> preDecls)
      <> [ ExternalFunc
             ( Function
                 { _funcReturnType = funReturnType ^. typeDeclType,
                   _funcIsPtr = funReturnType ^. typeIsPtr,
                   _funcQualifier = None,
                   _funcName = funcName,
                   _funcArgs = namedArgs asFunArg funArgTypes,
                   _funcBody = maybeToList (BodyStatement <$> mkBody bodySpec)
                 }
             )
         ]
      <> (ExternalMacro . CppDefineParens <$> toList nullaryDefine)
  where
    mkBody :: [(Maybe Expression, Statement)] -> Maybe Statement
    mkBody cs = do
      let lastBranch = const fallback . head <$> nonEmpty cs
      foldr mkIf lastBranch cs

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

    isNullary :: Bool
    isNullary = null funArgTypes && funcBasename /= Str.main_

    funcBasename :: Text
    funcBasename = mkName _funDefName

    nullaryDefine :: Maybe Define
    nullaryDefine =
      if
          | isNullary ->
              Just $
                Define
                  { _defineName = funcBasename,
                    _defineBody = functionCall (ExpressionVar funcName) []
                  }
          | otherwise -> Nothing
    funcName =
      if
          | isNullary -> asNullary funcBasename
          | otherwise -> funcBasename

    funArgTypes :: [CDeclType]
    funArgTypes = funType ^. cFunArgTypes

    funReturnType :: CDeclType
    funReturnType = funType ^. cFunReturnType

    funType :: CFunType
    funType = typeToFunType _funDefType

    fallback :: Statement
    fallback =
      StatementCompound
        [ StatementExpr
            ( functionCall
                (ExpressionVar Str.putStrLn_)
                [ ExpressionLiteral
                    ( LiteralString
                        ( "Error: Pattern match(es) are non-exhaustive in "
                            <> (_funDefName ^. Mono.nameText)
                        )
                    )
                ]
            ),
          StatementExpr
            ( functionCall
                (ExpressionVar Str.exit)
                [ ExpressionVar Str.exitFailure_
                ]
            )
        ]

type PatternBindings = HashMap Text (Expression, CFunType)

type FunType = ([CDeclType], CDeclType)

typeToFunType :: Mono.Type -> CFunType
typeToFunType t =
  let (_cFunArgTypes, _cFunReturnType) =
        second goType (first (map goType) (unfoldFunType t))
   in CFunType {..}

goFunctionClause ::
  forall r.
  Members '[Reader Mono.InfoTable, State EmittedClosures] r =>
  [Mono.Type] ->
  Mono.FunctionClause ->
  Sem r ((Maybe Expression, Statement), [Function])
goFunctionClause argTyps Mono.FunctionClause {..} = do
  (stmt, decls) <- returnStmt
  return ((clauseCondition, stmt), decls)
  where
    conditions :: [Expression]
    conditions = do
      (p, arg) <- zip _clausePatterns funArgs
      patternCondition (ExpressionVar arg) p

    patternCondition :: Expression -> Mono.Pattern -> [Expression]
    patternCondition arg = \case
      Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
        isCtor : subConditions
        where
          ctorName :: Text
          ctorName = mkName _constrAppConstructor

          isCtor :: Expression
          isCtor = functionCall (ExpressionVar (asIs ctorName)) [arg]

          asCtor :: Expression
          asCtor = functionCall (ExpressionVar (asCast ctorName)) [arg]
          subConditions :: [Expression]

          subConditions = do
            let subArgs = map (memberAccess Object asCtor) ctorArgs
            (p, subArg) <- zip _constrAppParameters subArgs
            patternCondition subArg p
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
    patternBindings :: Sem r PatternBindings
    patternBindings = HashMap.fromList <$> patternVars

    patternVars :: Sem r [(Text, (Expression, CFunType))]
    patternVars = concatMapM f (zipWith (curry (second (first ExpressionVar))) _clausePatterns (zip funArgs argTyps))
      where
        f (p, (arg, argTyp)) =
          case p of
            Mono.PatternVariable v -> return [(v ^. Mono.nameText, (arg, typeToFunType argTyp))]
            Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
              goConstructorApp arg _constrAppConstructor _constrAppParameters
            Mono.PatternWildcard {} -> return []

    returnStmt :: Sem r (Statement, [Function])
    returnStmt = do
      bindings <- patternBindings
      (decls :: [Function], clauseResult) <- runOutputList (runReader bindings (goExpression _clauseBody))
      return $ (StatementReturn (Just clauseResult), decls)

    goConstructorApp :: Expression -> Mono.Name -> [Mono.Pattern] -> Sem r [(Text, (Expression, CFunType))]
    goConstructorApp arg n ps = do
      ctorInfo' <- ctorInfo
      concatMapM f (zip ps (zip ctorArgs ctorInfo'))
      where
        ctorInfo :: Sem r [Mono.Type]
        ctorInfo = do
          p' :: HashMap Mono.Name Mono.ConstructorInfo <- asks (^. Mono.infoConstructors)
          let fInfo = HashMap.lookupDefault impossible n p'
          return $ fInfo ^. Mono.constructorInfoArgs

        f :: (Mono.Pattern, (Text, Mono.Type)) -> Sem r [(Text, (Expression, CFunType))]
        f (p, (field, fieldType)) = do
          let ctorField = memberAccess Object asConstructor field
          case p of
            Mono.PatternVariable v -> do
              return [(v ^. Mono.nameText, (ctorField, typeToFunType fieldType))]
            Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
              goConstructorApp ctorField _constrAppConstructor _constrAppParameters
            Mono.PatternWildcard {} -> return []
          where
            asConstructor :: Expression
            asConstructor = functionCall (ExpressionVar (asCast (mkName n))) [arg]

data ClosureInfo = ClosureInfo
  { _closureNameId :: Mono.NameId,
    _closureRootName :: Text,
    _closureFunType :: CFunType
  }
  deriving stock (Show, Eq)

genClosure :: ClosureInfo -> [Function]
genClosure ClosureInfo {..} =
  [ Function
      { _funcReturnType = returnType ^. typeDeclType,
        _funcIsPtr = returnType ^. typeIsPtr,
        _funcQualifier = None,
        _funcName = funName,
        _funcArgs = namedArgs asFunArg (declFunctionPtrType : argTypes),
        _funcBody =
          [ returnStatement
              ( functionCall
                  (ExpressionVar _closureRootName)
                  (ExpressionVar <$> (take (length argTypes) (drop 1 funArgs)))
              )
          ]
      },
    Function
      { _funcReturnType = declFunctionType,
        _funcIsPtr = True,
        _funcQualifier = None,
        _funcName = asNew funName,
        _funcArgs = [],
        _funcBody =
          [ BodyDecl
              ( Declaration
                  { _declType = declFunctionType,
                    _declIsPtr = True,
                    _declName = Just localName,
                    _declInitializer = Just $ ExprInitializer (mallocSizeOf Str.juvixFunctionT)
                  }
              ),
            BodyStatement
              ( StatementExpr
                  ( ExpressionAssign
                      ( Assign
                          { _assignLeft = memberAccess Pointer (ExpressionVar localName) "fun",
                            _assignRight =
                              castToType
                                ( CDeclType
                                    { _typeDeclType = uIntPtrType,
                                      _typeIsPtr = False
                                    }
                                )
                                (ExpressionVar funName)
                          }
                      )
                  )
              ),
            returnStatement (ExpressionVar localName)
          ]
      }
  ]
  where
    returnType :: CDeclType
    returnType = _closureFunType ^. cFunReturnType
    argTypes :: [CDeclType]
    argTypes = _closureFunType ^. cFunArgTypes
    localName :: Text
    localName = "f"
    funName :: Text
    funName = asFun _closureRootName

goExpression :: Members '[Reader Mono.InfoTable, Reader PatternBindings, Output Function, State EmittedClosures] r => Mono.Expression -> Sem r Expression
goExpression = \case
  Mono.ExpressionIden i -> do
    let rootFunMonoName = getName i
        rootFunNameId = rootFunMonoName ^. Mono.nameId
        rootFunName = mkName rootFunMonoName
        funName = asFun rootFunName
        newFunName = asNew funName

    case i of
      Mono.IdenVar {} -> goIden i
      _ -> do
        t <- getType i
        let argTyps = t ^. cFunArgTypes
        if
            | null argTyps -> goIden i
            | otherwise -> do
                emitted <- gets (HashSet.member rootFunNameId . (^. emittedNames))
                unless
                  emitted
                  ( do
                      modify (over emittedNames (HashSet.insert rootFunNameId))
                      mapM_
                        output
                        ( genClosure
                            ( ClosureInfo
                                { _closureNameId = rootFunNameId,
                                  _closureRootName = rootFunName,
                                  _closureFunType = t
                                }
                            )
                        )
                  )
                return $ functionCall (ExpressionVar newFunName) []
  Mono.ExpressionApplication a -> goApplication a
  Mono.ExpressionLiteral l -> return (ExpressionLiteral (goLiteral l))

getType ::
  Members '[Reader Mono.InfoTable, Reader PatternBindings] r =>
  Mono.Iden ->
  Sem r CFunType
getType = \case
  Mono.IdenFunction n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoFunctions)
    return $ typeToFunType (fInfo ^. Mono.functionInfoType)
  Mono.IdenConstructor n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoConstructors)
    return
      ( CFunType
          { _cFunArgTypes = goType <$> (fInfo ^. Mono.constructorInfoArgs),
            _cFunReturnType =
              goType
                (Mono.TypeIden (Mono.TypeIdenInductive (fInfo ^. Mono.constructorInfoInductive)))
          }
      )
  Mono.IdenAxiom n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoAxioms)
    return $ typeToFunType (fInfo ^. Mono.axiomInfoType)
  Mono.IdenVar n -> do
    p :: PatternBindings <- ask
    return $ snd (HashMap.lookupDefault impossible (n ^. Mono.nameText) p)

getName :: Mono.Iden -> Mono.Name
getName = \case
  Mono.IdenFunction n -> n
  Mono.IdenConstructor n -> n
  Mono.IdenVar n -> n
  Mono.IdenAxiom n -> n

goIden :: Members '[Reader PatternBindings, Reader Mono.InfoTable] r => Mono.Iden -> Sem r Expression
goIden = \case
  Mono.IdenFunction n -> return (ExpressionVar (mkName n))
  Mono.IdenConstructor n -> return (ExpressionVar (mkName n))
  Mono.IdenVar n -> do
    p :: PatternBindings <- ask
    return $ fst (HashMap.lookupDefault impossible (n ^. Mono.nameText) p)
  Mono.IdenAxiom n -> return (ExpressionVar (mkName n))

goApplication :: forall r. Members '[Reader PatternBindings, Reader Mono.InfoTable, Output Function, State EmittedClosures] r => Mono.Application -> Sem r Expression
goApplication a = do
  (iden, fArgs) <- f

  case iden of
    Mono.IdenVar n -> do
      p :: PatternBindings <- ask
      let (idenExp, idenTyp) = HashMap.lookupDefault impossible (n ^. Mono.nameText) p
      return $ juvixFunctionCall idenTyp idenExp (reverse fArgs)
    _ -> do
      idenExp <- goIden iden
      return $ functionCall idenExp (reverse fArgs)
  where
    f :: Sem r (Mono.Iden, [Expression])
    f = unfoldApp a
    unfoldApp :: Mono.Application -> Sem r (Mono.Iden, [Expression])
    unfoldApp Mono.Application {..} = case _appLeft of
      Mono.ExpressionApplication x -> do
        fName <- goExpression _appRight
        uf <- unfoldApp x
        return (second (fName :) uf)
      Mono.ExpressionIden i -> do
        fArg <- goExpression _appRight
        return (i, [fArg])
      Mono.ExpressionLiteral {} -> impossible

goLiteral :: C.LiteralLoc -> Literal
goLiteral C.LiteralLoc {..} = case _literalLocLiteral of
  C.LitString s -> LiteralString s
  C.LitInteger i -> LiteralInt i

goAxiom ::
  Member (Reader Mono.CompileInfoTable) r =>
  Mono.AxiomDef ->
  Sem r [CCode]
goAxiom a = do
  backends <- lookupBackends (axiomName ^. Mono.nameId)
  case firstJust getCode backends of
    Nothing -> error ("C backend does not support this axiom:" <> show (axiomName ^. Mono.nameText))
    Just defineBody ->
      return
        [ ExternalMacro
            ( CppDefine
                ( Define
                    { _defineName = defineName,
                      _defineBody = ExpressionVar defineBody
                    }
                )
            )
        ]
  where
    axiomName :: Mono.Name
    axiomName = a ^. Mono.axiomName
    defineName :: Text
    defineName = mkName axiomName
    getCode :: BackendItem -> Maybe Text
    getCode b =
      guard (BackendC == b ^. backendItemBackend)
        $> b ^. backendItemCode
    lookupBackends ::
      Member (Reader Mono.CompileInfoTable) r =>
      NameId ->
      Sem r [BackendItem]
    lookupBackends f = (^. S.compileInfoBackendItems) . HashMap.lookupDefault impossible f <$> ask

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
                  [ typeDefType (asTag baseName) Str.tag,
                    Declaration
                      { _declType = unionMembers,
                        _declIsPtr = False,
                        _declName = Just Str.data_,
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
              _structMembers = Just (map (\ctorName -> typeDefType (asTypeDef ctorName) (asField ctorName)) constructorNames)
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
                    (memberAccess Pointer (ExpressionVar funcArg) Str.tag)
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
                (memberAccess Object (memberAccess Pointer (ExpressionVar funcArg) Str.data_) (asField ctorName))
            ]
        }
      where
        funcArg :: Text
        funcArg = "a"

goInductiveConstructorNew ::
  Mono.InductiveDef ->
  Mono.InductiveConstructorDef ->
  [CCode]
goInductiveConstructorNew i ctor = ctorNewFun
  where
    ctorNewFun :: [CCode]
    ctorNewFun = if null ctorParams then ctorNewNullary else ctorNewNary

    baseName :: Text
    baseName = mkName (ctor ^. Mono.constructorName)

    inductiveName :: Text
    inductiveName = mkInductiveName i

    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Mono.constructorParameters

    ctorNewNullary :: [CCode]
    ctorNewNullary =
      [ ExternalFunc $
          commonFunctionDeclr
            (asNullary baseName)
            []
            [ BodyDecl allocInductive,
              BodyDecl (commonInitDecl (dataInit Str.true_)),
              BodyStatement assignPtr,
              returnStatement (ExpressionVar tmpPtrName)
            ],
        ExternalMacro
          ( CppDefineParens
              ( Define
                  { _defineName = baseName,
                    _defineBody = functionCall (ExpressionVar (asNullary baseName)) []
                  }
              )
          )
      ]

    ctorNewNary :: [CCode]
    ctorNewNary =
      [ ExternalFunc $
          commonFunctionDeclr
            baseName
            ctorDecls
            [ BodyDecl allocInductive,
              BodyDecl ctorStructInit,
              BodyDecl (commonInitDecl (dataInit tmpCtorStructName)),
              BodyStatement assignPtr,
              returnStatement (ExpressionVar tmpPtrName)
            ]
      ]
      where
        ctorDecls :: [Declaration]
        ctorDecls = inductiveCtorArgs ctor

        ctorInit :: [DesigInit]
        -- TODO: _declName is never Nothing by construction, fix the types
        ctorInit = map (f . fromJust . (^. declName)) ctorDecls

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

    commonFunctionDeclr :: Text -> [Declaration] -> [BodyItem] -> Function
    commonFunctionDeclr name args body =
      Function
        { _funcReturnType = DeclTypeDefType (asTypeDef inductiveName),
          _funcIsPtr = True,
          _funcQualifier = StaticInline,
          _funcName = name,
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
                        { _desigDesignator = Str.tag,
                          _desigInitializer = ExprInitializer (ExpressionVar (asTag baseName))
                        },
                      DesigInit
                        { _desigDesignator = Str.data_,
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
            { _desigDesignator = asField baseName,
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

namedArgs :: (Text -> Text) -> [CDeclType] -> [Declaration]
namedArgs prefix = zipWith goTypeDecl argLabels
  where
    argLabels :: [Text]
    argLabels = prefix . show <$> [0 :: Integer ..]

inductiveCtorArgs :: Mono.InductiveConstructorDef -> [Declaration]
inductiveCtorArgs ctor = namedArgs asCtorArg (goType <$> ctorParams)
  where
    ctorParams :: [Mono.Type]
    ctorParams = ctor ^. Mono.constructorParameters

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
    ctorParams = ctor ^. Mono.constructorParameters

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

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Mono.Type -> ([Mono.Type], Mono.Type)
unfoldFunType t = case t of
  Mono.TypeFunction (Mono.Function l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

goType :: Mono.Type -> CDeclType
goType t = case t of
  Mono.TypeIden ti -> getMonoType ti
  Mono.TypeFunction {} -> declFunctionPtrType
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
goTypeDecl n CDeclType {..} =
  Declaration
    { _declType = _typeDeclType,
      _declIsPtr = _typeIsPtr,
      _declName = Just n,
      _declInitializer = Nothing
    }

goTypeDecl'' :: CDeclType -> Declaration
goTypeDecl'' CDeclType {..} =
  Declaration
    { _declType = _typeDeclType,
      _declIsPtr = _typeIsPtr,
      _declName = Nothing,
      _declInitializer = Nothing
    }

mallocSizeOf :: Text -> Expression
mallocSizeOf typeName =
  functionCall (ExpressionVar Str.malloc) [functionCall (ExpressionVar Str.sizeof) [ExpressionVar typeName]]

declFunctionType :: DeclType
declFunctionType = DeclTypeDefType Str.juvixFunctionT

declFunctionPtrType :: CDeclType
declFunctionPtrType =
  CDeclType
    { _typeDeclType = declFunctionType,
      _typeIsPtr = True
    }

funPtrType :: CFunType -> CDeclType
funPtrType CFunType {..} =
  CDeclType
    { _typeDeclType =
        DeclFunPtr
          ( FunPtr
              { _funPtrReturnType = _cFunReturnType ^. typeDeclType,
                _funPtrIsPtr = _cFunReturnType ^. typeIsPtr,
                _funPtrArgs = _cFunArgTypes
              }
          ),
      _typeIsPtr = False
    }

juvixFunctionCall :: CFunType -> Expression -> [Expression] -> Expression
juvixFunctionCall funType funParam args =
  functionCall (castToType (funPtrType fTyp) (memberAccess Pointer funParam "fun")) (funParam : args)
  where
    fTyp :: CFunType
    fTyp = funType {_cFunArgTypes = declFunctionPtrType : (funType ^. cFunArgTypes)}
