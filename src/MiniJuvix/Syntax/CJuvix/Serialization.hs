module MiniJuvix.Syntax.CJuvix.Serialization where

import Language.C qualified as C
import Language.C.Data.Ident qualified as C
import Language.C.Pretty qualified as P
import Language.C.Syntax
import MiniJuvix.Prelude
import MiniJuvix.Syntax.CJuvix.Language
import Text.PrettyPrint.HughesPJ qualified as HP

encAngles :: HP.Doc -> HP.Doc
encAngles p = HP.char '<' HP.<> p HP.<> HP.char '>'

instance P.Pretty Cpp where
  pretty = \case
    CppIncludeFile i -> "#include" HP.<+> HP.doubleQuotes (P.pretty i)
    CppIncludeSystem i -> "#include" HP.<+> encAngles (P.pretty i)
    CppDefine (Define {..}) -> "#define" HP.<+> (P.pretty _defineName HP.<+> P.pretty _defineBody)

instance P.Pretty Text where
  pretty = HP.text . unpack

instance P.Pretty CCodeUnit where
  pretty c = HP.vcat (map P.pretty (c ^. ccodeCode))

instance P.Pretty CCode where
  pretty = \case
    ExternalDecl decl -> P.pretty (CDeclExt (mkCDecl decl))
    ExternalFunc fun -> P.pretty (CFDefExt (mkCFunDef fun))
    ExternalMacro m -> P.pretty m
    Verbatim t -> P.pretty t

serialize :: CCodeUnit -> Text
serialize = show . P.pretty

mkCDecl :: Declaration -> CDecl
mkCDecl Declaration {..} =
  CDecl
    (mkDeclSpecifier _declType)
    [(Just declrName, initializer, Nothing)]
    C.undefNode
  where
    declrName :: CDeclr
    declrName = CDeclr (mkIdent <$> _declName) ptrDeclr Nothing [] C.undefNode
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
