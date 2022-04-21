module MiniJuvix.Syntax.CJuvix.Serialization where

import Language.C qualified as C
import Language.C.Data.Ident qualified as C
import Language.C.Pretty qualified as P
import Language.C.Syntax
import MiniJuvix.Prelude
import MiniJuvix.Syntax.CJuvix.Language

serialize :: CCode -> Text
serialize (CCode {..}) = pack (show (P.pretty tUnit))
  where
    tUnit :: CTranslUnit
    tUnit = CTranslUnit (f <$> _ccodeExternal) C.undefNode
    f :: External -> CExtDecl
    f = \case
      ExternalDecl decl -> CDeclExt (mkCDecl decl)
      ExternalFunc fun -> CFDefExt (mkCFunDef fun)

mkCDecl :: Declaration -> CDecl
mkCDecl Declaration {..} =
  CDecl
    (maybe [] mkDeclSpecifier _declType)
    [(Just declrName, initializer, Nothing)]
    C.undefNode
  where
    declrName :: CDeclr
    declrName = CDeclr (Just (mkIdent _declName)) ptrDeclr Nothing [] C.undefNode
    ptrDeclr :: [CDerivedDeclarator C.NodeInfo]
    ptrDeclr = [CPtrDeclr [] C.undefNode | _declIsPtr]
    initializer :: Maybe CInit
    initializer = mkCInit <$> _declInitializer

mkCInit :: Initializer -> CInit
mkCInit = \case
  ExprInitializer e -> CInitExpr (mkCExpr e) C.undefNode
  DesignatorInitializer ds -> CInitList (f <$> ds) C.undefNode
  where
    f :: DesigInit -> ([CDesignator], CInit)
    f (DesigInit {..}) = ([CMemberDesig (mkIdent _desigDesignator) C.undefNode], mkCInit _desigInitializer)

mkCFunDef :: Function -> CFunDef
mkCFunDef (Function {..}) =
  CFunDef declSpec declr [] statement C.undefNode
  where
    declr :: CDeclr
    declr = CDeclr (Just (mkIdent _funcName)) derivedDeclr Nothing [] C.undefNode
    declSpec :: [CDeclSpec]
    declSpec = qualifier <> mkDeclSpecifier _funcReturnType
    qualifier :: [CDeclSpec]
    qualifier = if _funcQualifier == StaticInline then [CStorageSpec (CStatic C.undefNode), CFunSpec (CInlineQual C.undefNode)] else []
    derivedDeclr :: [CDerivedDeclr]
    derivedDeclr = funDerDeclr <> ptrDeclr
    ptrDeclr :: [CDerivedDeclr]
    ptrDeclr = [CPtrDeclr [] C.undefNode | _funcIsPtr]
    funDerDeclr :: [CDerivedDeclr]
    funDerDeclr = [CFunDeclr (Right (funArgs, False)) [] C.undefNode]
    funArgs :: [CDecl]
    funArgs = mkCDecl <$> _funcArgs
    statement :: CStat
    statement = CCompound [] block C.undefNode
    block :: [CBlockItem]
    block = mkBlockItem <$> _funcBody

mkBlockItem :: BodyItem -> CBlockItem
mkBlockItem = \case
  BodyStatement s -> CBlockStmt (mkCStat s)
  BodyDecl d -> CBlockDecl (mkCDecl d)

mkCExpr :: Expression -> CExpr
mkCExpr = \case
  ExpressionAssign (Assign {..}) -> CAssign CAssignOp (mkCExpr _assignLeft) (mkCExpr _assignRight) C.undefNode
  ExpressionCast (Cast {..}) -> CCast (mkCDecl _castDecl) (mkCExpr _castExpression) C.undefNode
  ExpressionCall (Call {..}) -> CCall (mkCExpr _callCallee) (mkCExpr <$> _callArgs) C.undefNode
  ExpressionLiteral l -> case l of
    LiteralInt i -> CConst (CIntConst (cInteger i) C.undefNode)
    LiteralChar c -> CConst (CCharConst (cChar c) C.undefNode)
    LiteralString s -> CConst (CStrConst (cString (unpack s)) C.undefNode)
  ExpressionVar n -> CVar (mkIdent n) C.undefNode
  ExpressionBinary (Binary {..}) ->
    CBinary (mkBinaryOp _binaryOp) (mkCExpr _binaryLeft) (mkCExpr _binaryRight) C.undefNode
  ExpressionUnary (Unary {..}) ->
    CUnary (mkUnaryOp _unaryOp) (mkCExpr _unarySubject) C.undefNode
  ExpressionMember (MemberAccess {..}) ->
    CMember (mkCExpr _memberSubject) (mkIdent _memberField) (_memberOp == Pointer) C.undefNode

mkCStat :: Statement -> CStat
mkCStat = \case
  StatementReturn me -> CReturn (mkCExpr <$> me) C.undefNode
  StatementIf (If {..}) ->
    CIf (mkCExpr _ifCondition) (mkCStat _ifThen) (mkCStat <$> _ifElse) C.undefNode
  StatementExpr e -> CExpr (Just (mkCExpr e)) C.undefNode

mkBinaryOp :: BinaryOp -> CBinaryOp
mkBinaryOp = \case
  Eq -> CEqOp
  Neq -> CNeqOp
  And -> CLndOp
  Or -> CLorOp

mkUnaryOp :: UnaryOp -> CUnaryOp
mkUnaryOp = \case
  Address -> CAdrOp
  Indirection -> CIndOp
  Negation -> CNegOp

mkDeclSpecifier :: DeclType -> [CDeclSpec]
mkDeclSpecifier = \case
  DeclTypeDefType typeDefName -> mkTypeDefTypeSpec typeDefName
  DeclTypeDef declType -> CStorageSpec (CTypedef C.undefNode) : mkDeclSpecifier declType
  DeclStructUnion (StructUnion {..}) -> mkStructUnionTypeSpec _structUnionTag _structUnionName _structMembers
  DeclEnum (Enum {..}) -> mkEnumSpec _enumName _enumMembers
  BoolType -> [CTypeSpec (CBoolType C.undefNode)]

mkEnumSpec :: Maybe Text -> Maybe [Text] -> [CDeclSpec]
mkEnumSpec name members = [CTypeSpec (CEnumType enum C.undefNode)]
  where
    enum :: CEnum
    enum = CEnum (mkIdent <$> name) (fmap (map (\m -> (mkIdent m, Nothing))) members) [] C.undefNode

mkTypeDefTypeSpec :: Text -> [CDeclSpec]
mkTypeDefTypeSpec name = [CTypeSpec (CTypeDef (mkIdent name) C.undefNode)]

mkStructUnionTypeSpec :: StructUnionTag -> Maybe Text -> Maybe [Declaration] -> [CDeclSpec]
mkStructUnionTypeSpec tag name members = [CTypeSpec (CSUType struct C.undefNode)]
  where
    struct :: CStructUnion
    struct = CStruct cStructTag (mkIdent <$> name) memberDecls [] C.undefNode
    memberDecls :: Maybe [CDecl]
    memberDecls = fmap (map mkCDecl) members
    cStructTag = case tag of
      StructTag -> CStructTag
      UnionTag -> CUnionTag

mkIdent :: Text -> C.Ident
mkIdent t = C.Ident (unpack t) 0 C.undefNode

example1 :: Declaration
example1 =
  Declaration
    { _declType = Just t,
      _declIsPtr = True,
      _declName = "n",
      _declInitializer = Just i
    }
  where
    t = DeclTypeDefType "nat_t"
    i =
      ExprInitializer
        ( ExpressionCall
            ( Call
                { _callCallee = ExpressionVar "malloc",
                  _callArgs =
                    [ ExpressionCall
                        ( Call
                            { _callCallee = ExpressionVar "sizeof",
                              _callArgs = [ExpressionVar "nat_t"]
                            }
                        )
                    ]
                }
            )
        )

example2 :: Expression
example2 =
  ExpressionBinary
    ( Binary
        { _binaryOp = Eq,
          _binaryLeft = l,
          _binaryRight = r
        }
    )
  where
    l =
      ExpressionMember
        ( MemberAccess
            { _memberSubject = ExpressionVar "n",
              _memberField = "tag",
              _memberOp = Pointer
            }
        )
    r = ExpressionVar "nat_ctor_tag_zero"

example3 :: Function
example3 =
  Function
    { _funcReturnType = BoolType,
      _funcIsPtr = True,
      _funcQualifier = StaticInline,
      _funcName = "nat_is_zero",
      _funcArgs =
        [ Declaration
            { _declType = Just (DeclTypeDefType "nat_t"),
              _declIsPtr = True,
              _declName = "n",
              _declInitializer = Nothing
            }
        ],
      _funcBody = [BodyStatement (StatementReturn (Just example2))]
    }

example4 :: Function
example4 =
  Function
    { _funcReturnType = DeclTypeDefType "nat_t",
      _funcIsPtr = True,
      _funcQualifier = StaticInline,
      _funcName = "nat_new_zero",
      _funcArgs = [],
      _funcBody =
        [ BodyDecl
            ( Declaration
                { _declType = Just (DeclTypeDefType "nat_t"),
                  _declIsPtr = True,
                  _declName = "n",
                  _declInitializer =
                    Just
                      ( ExprInitializer
                          ( ExpressionCall
                              ( Call
                                  { _callCallee = ExpressionVar "malloc",
                                    _callArgs =
                                      [ ExpressionCall
                                          ( Call
                                              { _callCallee = ExpressionVar "sizeof",
                                                _callArgs = [ExpressionVar "nat_t"]
                                              }
                                          )
                                      ]
                                  }
                              )
                          )
                      )
                }
            ),
          BodyDecl
            ( Declaration
                { _declType = Just (DeclTypeDefType "nat_t"),
                  _declIsPtr = False,
                  _declName = "m",
                  _declInitializer =
                    Just
                      ( DesignatorInitializer
                          [ DesigInit
                              { _desigDesignator = "tag",
                                _desigInitializer =
                                  ExprInitializer
                                    (ExpressionVar "nat_ctor_tag_zero")
                              },
                            DesigInit
                              { _desigDesignator = "data",
                                _desigInitializer =
                                  DesignatorInitializer
                                    [ DesigInit
                                        { _desigDesignator = "zero",
                                          _desigInitializer =
                                            ExprInitializer
                                              (ExpressionVar "true")
                                        }
                                    ]
                              }
                          ]
                      )
                }
            ),
          BodyStatement
            ( StatementExpr
                ( ExpressionAssign
                    ( Assign
                        { _assignLeft =
                            ExpressionUnary
                              ( Unary
                                  { _unaryOp = Indirection,
                                    _unarySubject = ExpressionVar "n"
                                  }
                              ),
                          _assignRight = ExpressionVar "m"
                        }
                    )
                )
            ),
          BodyStatement
            (StatementReturn (Just (ExpressionVar "n")))
        ]
    }
