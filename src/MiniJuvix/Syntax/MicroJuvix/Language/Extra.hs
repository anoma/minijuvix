module MiniJuvix.Syntax.MicroJuvix.Language.Extra
  ( module MiniJuvix.Syntax.MicroJuvix.Language.Extra,
    module MiniJuvix.Syntax.MicroJuvix.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

data Caller
  = CallerInductive InductiveName
  | CallerFunction FunctionName
  | CallerAxiom AxiomName
  deriving stock (Eq, Ord, Generic)

data TypeCallIden
  = InductiveIden InductiveName
  | FunctionIden FunctionName
  deriving stock (Eq, Ord, Generic)

data TypeCall' a = TypeCall'
  { _typeCallIden :: TypeCallIden,
    _typeCallArguments :: NonEmpty a
  }
  deriving stock (Eq, Generic)

newtype TypeCallsMap = TypeCallsMap
  { _typeCallsMap :: HashMap Caller (HashSet TypeCall)
  }

instance Functor TypeCall' where
  fmap f (TypeCall' i args) = TypeCall' i (fmap f args)

newtype ConcreteType = ConcreteType {_unconcreteType :: Expression}
  deriving stock (Eq, Generic)

type ConcreteTypeCall = TypeCall' ConcreteType

type TypeCall = TypeCall' Expression

type SubsE = HashMap VarName Expression

type Rename = HashMap VarName VarName

type Subs = HashMap VarName Expression

type ConcreteSubs = HashMap VarName ConcreteType

-- | Indexed by _typeCallIden
newtype TypeCalls = TypeCalls
  { _typeCallSet :: HashMap TypeCallIden (HashMap ConcreteTypeCall ConcreteSubs)
  }

type VarMap = HashMap VarName VarName

emptyCalls :: TypeCalls
emptyCalls = TypeCalls mempty

instance Hashable TypeCallIden

instance Hashable TypeCall

instance Hashable Caller

instance Hashable ConcreteTypeCall

instance Hashable ConcreteType

makeLenses ''TypeCalls
makeLenses ''TypeCall'
makeLenses ''TypeCallsMap
makeLenses ''ConcreteType

typeCallIdenToCaller :: TypeCallIden -> Caller
typeCallIdenToCaller = \case
  InductiveIden i -> CallerInductive i
  FunctionIden i -> CallerFunction i

mkConcreteType' :: Expression -> ConcreteType
mkConcreteType' =
  fromMaybe (error "the given type is not concrete")
    . mkConcreteType

-- TODO: consider using traversals to simplify
mkConcreteType :: Expression -> Maybe ConcreteType
mkConcreteType = fmap ConcreteType . go
  where
    go :: Expression -> Maybe Expression
    go t = case t of
      ExpressionApplication (Application l r i) -> do
        l' <- go l
        r' <- go r
        return (ExpressionApplication (Application l' r' i))
      ExpressionUniverse {} -> return t
      ExpressionFunction {} -> impossible
      ExpressionFunction2 (Function2 l r) -> do
        l' <- goParam l
        r' <- go r
        return (ExpressionFunction2 (Function2 l' r'))
      ExpressionHole {} -> Nothing
      ExpressionLiteral {} -> return t
      ExpressionIden i -> case i of
        IdenFunction {} -> return t
        IdenInductive {} -> return t
        IdenConstructor {} -> return t
        IdenAxiom {} -> return t
        IdenVar {} -> Nothing
    goParam :: FunctionParameter -> Maybe FunctionParameter
    goParam (FunctionParameter m i e) = do
      guard (isNothing m)
      e' <- go e
      return (FunctionParameter m i e')

class HasExpressions a where
  expressions :: Traversal' a Expression

instance HasExpressions Expression where
  expressions f e = case e of
    ExpressionIden {} -> pure e
    ExpressionApplication a -> ExpressionApplication <$> expressions f a
    ExpressionFunction {} -> impossible
    ExpressionFunction2 fun -> ExpressionFunction2 <$> expressions f fun
    ExpressionLiteral {} -> pure e
    ExpressionUniverse {} -> pure e
    ExpressionHole {} -> pure e

instance HasExpressions FunctionParameter where
  expressions f (FunctionParameter m i e) = do
    e' <- expressions f e
    pure (FunctionParameter m i e')

instance HasExpressions Function2 where
  expressions f (Function2 l r) = do
    l' <- expressions f l
    r' <- expressions f r
    pure (Function2 l' r')

instance HasExpressions Application where
  expressions f (Application l r i) = do
    l' <- expressions f l
    r' <- expressions f r
    pure (Application l' r' i)

-- | Prism
_ExpressionHole :: Traversal' Expression Hole
_ExpressionHole f e = case e of
  ExpressionIden {} -> pure e
  ExpressionApplication {} -> pure e
  ExpressionFunction {} -> impossible
  ExpressionFunction2 {} -> pure e
  ExpressionLiteral {} -> pure e
  ExpressionUniverse {} -> pure e
  ExpressionHole h -> ExpressionHole <$> f h

holes :: HasExpressions a => Traversal' a Hole
holes = expressions . _ExpressionHole

hasHoles :: HasExpressions a => a -> Bool
hasHoles = has holes

subsHoles :: HasExpressions a => HashMap Hole Expression -> a -> a
subsHoles s = over expressions helper
  where
  helper :: Expression -> Expression
  helper e = case e of
    ExpressionHole h -> fromMaybe e (s ^. at h)
    _ -> e

instance HasExpressions FunctionClause where
  expressions f (FunctionClause n ps b) = do
    b' <- expressions f b
    pure (FunctionClause n ps b')

instance HasExpressions FunctionDef where
  expressions f (FunctionDef name ty clauses bi) = do
    clauses' <- traverse (expressions f) clauses
    return (FunctionDef name ty clauses' bi)

fillHolesFunctionDef :: HashMap Hole Expression -> FunctionDef -> FunctionDef
fillHolesFunctionDef = subsHoles

fillHolesClause :: HashMap Hole Expression -> FunctionClause -> FunctionClause
fillHolesClause = subsHoles

fillHoles :: HashMap Hole Expression -> Expression -> Expression
fillHoles = subsHoles

substituteIndParams :: [(InductiveParameter, Expression)] -> Expression -> Expression
substituteIndParams = substitutionE . HashMap.fromList . map (first (^. inductiveParamName))

typeAbstraction :: Name -> FunctionParameter
typeAbstraction var = FunctionParameter (Just var) Explicit (ExpressionUniverse (SmallUniverse (getLoc var)))

unnamedParameter :: Expression -> FunctionParameter
unnamedParameter = FunctionParameter Nothing Explicit

-- substitutionArg :: VarName -> VarName -> FunctionParameter -> FunctionParameter
-- substitutionArg from v a = case a of
--   FunctionArgTypeAbstraction {} -> a
--   FunctionArgTypeType ty ->
--     FunctionArgTypeType
--       (substitutionE (HashMap.singleton from (ExpressionIden (IdenVar v))) ty)

renameToSubsE :: Rename -> SubsE
renameToSubsE = fmap (ExpressionIden . IdenVar)

renameExpression :: Rename -> Expression -> Expression
renameExpression r = substitutionE (renameToSubsE r)

patternVariables :: Pattern -> [VarName]
patternVariables = \case
  PatternVariable v -> [v]
  PatternConstructorApp a -> goApp a
  PatternBraces b -> patternVariables b
  PatternWildcard {} -> []
  where
    goApp :: ConstructorApp -> [VarName]
    goApp (ConstructorApp _ ps) = concatMap patternVariables ps

renamePattern :: Rename -> Pattern -> Pattern
renamePattern m = go
  where
    go :: Pattern -> Pattern
    go p = case p of
      PatternVariable v
        | Just v' <- m ^. at v -> PatternVariable v'
      PatternConstructorApp a -> PatternConstructorApp (goApp a)
      _ -> p
    goApp :: ConstructorApp -> ConstructorApp
    goApp (ConstructorApp c ps) = ConstructorApp c (map go ps)

inductiveTypeVarsAssoc :: Foldable f => InductiveDef -> f a -> HashMap VarName a
inductiveTypeVarsAssoc def l
  | length vars < n = impossible
  | otherwise = HashMap.fromList (zip vars (toList l))
  where
    n = length l
    vars :: [VarName]
    vars = def ^.. inductiveParameters . each . inductiveParamName

functionTypeVarsAssoc :: forall a f. Foldable f => FunctionDef -> f a -> HashMap VarName a
functionTypeVarsAssoc def l = sig <> mconcatMap clause (def ^. funDefClauses)
  where
    n = length l
    zipl :: [Maybe VarName] -> HashMap VarName a
    zipl x = HashMap.fromList (mapMaybe aux (zip x (toList l)))
      where
        aux :: (Maybe x, y) -> Maybe (x, y)
        aux = \case
          (Just a, b) -> Just (a, b)
          _ -> Nothing
    sig
      | length tyVars < n = impossible
      | otherwise = zipl (map Just tyVars)
      where
        tyVars = fst (unfoldTypeAbsType (def ^. funDefType))
    clause :: FunctionClause -> HashMap VarName a
    clause c = zipl clauseVars
      where
        clauseVars :: [Maybe VarName]
        clauseVars = take n (map patternVar (c ^. clausePatterns))
          where
            patternVar :: Pattern -> Maybe VarName
            patternVar = \case
              PatternVariable v -> Just v
              _ -> Nothing

substitutionConcrete :: ConcreteSubs -> Expression -> ConcreteType
substitutionConcrete m = mkConcreteType' . substitutionE ((^. unconcreteType) <$> m)

concreteTypeToExpr :: ConcreteType -> Expression
concreteTypeToExpr = (^. unconcreteType)

concreteSubsToSubsE :: ConcreteSubs -> SubsE
concreteSubsToSubsE = fmap concreteTypeToExpr

substitutionApp :: (Maybe Name, Expression) -> Expression -> Expression
substitutionApp (mv, ty) = case mv of
  Nothing -> id
  Just v -> substitutionE (HashMap.singleton v ty)

substitutionE :: SubsE -> Expression -> Expression
substitutionE m = go
  where
    go :: Expression -> Expression
    go x = case x of
      ExpressionIden i -> goIden i
      ExpressionApplication a -> ExpressionApplication (goApp a)
      ExpressionFunction2 a -> ExpressionFunction2 (goAbs a)
      ExpressionLiteral {} -> x
      ExpressionUniverse {} -> x
      ExpressionHole {} -> x
      ExpressionFunction f -> ExpressionFunction (goFunction f)

    goParam :: FunctionParameter -> FunctionParameter
    goParam (FunctionParameter v i ty) = FunctionParameter v i (go ty)
    goAbs :: Function2 -> Function2
    goAbs (Function2 l r) = Function2 (goParam l) (go r)
    goApp :: Application -> Application
    goApp (Application l r i) = Application (go l) (go r) i
    goFunction :: FunctionExpression -> FunctionExpression
    goFunction (FunctionExpression l r) = FunctionExpression (go l) (go r)
    goIden :: Iden -> Expression
    goIden i = case i of
      IdenVar v
        | Just e <- HashMap.lookup v m -> e
      _ -> ExpressionIden i

smallUniverse :: Interval -> Expression
smallUniverse = ExpressionUniverse . SmallUniverse

-- fromFunctionArgType :: FunctionArgType -> FunctionParameter
-- fromFunctionArgType = \case
--   FunctionArgTypeAbstraction (i, var) ->
--     FunctionParameter (Just var) i (smallUniverse (getLoc var))
--   FunctionArgTypeType t ->
--     FunctionParameter Nothing Explicit t

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [FunctionParameter] -> Expression -> Expression
foldFunType l r = case l of
  [] -> r
  (a : as) ->
    let r' = foldFunType as r
    in ExpressionFunction2 (Function2 a r')

-- -- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Expression -> ([FunctionParameter], Expression)
unfoldFunType t = case t of
  ExpressionFunction2 (Function2 l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

-- unfoldFunConcreteType :: ConcreteType -> ([ConcreteType], ConcreteType)
-- unfoldFunConcreteType = bimap (map mkConcreteType') mkConcreteType' . go . (^. unconcreteType)
--   where
--     go :: Type -> ([Type], Type)
--     go t = case t of
--       TypeFunction (Function l r) -> first (l :) (go r)
--       _ -> ([], t)

unfoldTypeAbsType :: Expression -> ([VarName], Expression)
unfoldTypeAbsType t = case t of
  ExpressionFunction2 (Function2 (FunctionParameter (Just var) _ _) r) ->
    first (var :) (unfoldTypeAbsType r)
  _ -> ([], t)

foldExplicitApplication :: Expression -> [Expression] -> Expression
foldExplicitApplication f = foldApplication f . zip (repeat Explicit)

foldApplication :: Expression -> [(IsImplicit, Expression)] -> Expression
foldApplication f = \case
  [] -> f
  ((i, a) : as) -> foldApplication (ExpressionApplication (Application f a i)) as

unfoldApplication' :: Application -> (Expression, NonEmpty (IsImplicit, Expression))
unfoldApplication' (Application l' r' i') = second (|: (i', r')) (unfoldExpression l')
  where
    unfoldExpression :: Expression -> (Expression, [(IsImplicit, Expression)])
    unfoldExpression e = case e of
      ExpressionApplication (Application l r i) ->
        second (`snoc` (i, r)) (unfoldExpression l)
      _ -> (e, [])

unfoldApplication :: Application -> (Expression, NonEmpty Expression)
unfoldApplication = fmap (fmap snd) . unfoldApplication'

-- unfoldType :: Type -> (Type, [Type])
-- unfoldType = \case
--   TypeApp (TypeApplication l r _) -> second (`snoc` r) (unfoldType l)
--   t -> (t, [])

-- foldTypeApp :: Type -> [Type] -> Type
-- foldTypeApp ty = \case
--   [] -> ty
--   (p : ps) -> foldTypeApp (TypeApp (TypeApplication ty p Explicit)) ps

-- foldTypeAppName :: Name -> [Name] -> Type
-- foldTypeAppName tyName indParams =
--   foldTypeApp
--     (TypeIden (TypeIdenInductive tyName))
--     (map (TypeIden . TypeIdenVariable) indParams)

-- getTypeName :: Type -> Maybe Name
-- getTypeName = \case
--   (TypeIden (TypeIdenInductive tyName)) -> Just tyName
--   (TypeApp (TypeApplication l _ _)) -> getTypeName l
--   _ -> Nothing

reachableModules :: Module -> [Module]
reachableModules = fst . run . runOutputList . evalState (mempty :: HashSet Name) . go
  where
    go :: forall r. Members '[State (HashSet Name), Output Module] r => Module -> Sem r ()
    go m = do
      s <- get
      unless
        (HashSet.member (m ^. moduleName) s)
        (output m >> goBody (m ^. moduleBody))
      where
        goBody :: ModuleBody -> Sem r ()
        goBody = mapM_ goStatement . (^. moduleStatements)
        goStatement :: Statement -> Sem r ()
        goStatement = \case
          StatementInclude (Include inc) -> go inc
          _ -> return ()
