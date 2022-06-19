module MiniJuvix.Syntax.Abstract.Language.Extra
  ( module MiniJuvix.Syntax.Abstract.Language,
    module MiniJuvix.Syntax.Abstract.Language.Extra,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language

patternVariables :: Pattern -> [VarName]
patternVariables = \case
  PatternVariable v -> [v]
  PatternWildcard {} -> []
  PatternEmpty {} -> []
  PatternBraces b -> patternVariables b
  PatternConstructorApp app -> appVariables app

appVariables :: ConstructorApp -> [VarName]
appVariables (ConstructorApp _ ps) = concatMap patternVariables ps

idenName :: Iden -> Name
idenName = \case
  IdenFunction (FunctionRef f) -> f
  IdenConstructor (ConstructorRef c) -> c
  IdenVar v -> v
  IdenInductive (InductiveRef i) -> i
  IdenAxiom (AxiomRef a) -> a

smallerPatternVariables :: Pattern -> [VarName]
smallerPatternVariables = \case
  PatternVariable {} -> []
  PatternBraces b -> smallerPatternVariables b
  PatternWildcard {} -> []
  PatternEmpty {} -> []
  PatternConstructorApp app -> appVariables app

viewApp :: Expression -> (Expression, [Expression])
viewApp e = case e of
  ExpressionApplication (Application l r _) ->
    second (`snoc` r) (viewApp l)
  _ -> (e, [])

viewExpressionAsPattern :: Expression -> Maybe Pattern
viewExpressionAsPattern e = case viewApp e of
  (f, args)
    | Just c <- getConstructor f -> do
        args' <- mapM viewExpressionAsPattern args
        Just $ PatternConstructorApp (ConstructorApp c args')
  (f, [])
    | Just v <- getVariable f -> Just (PatternVariable v)
  _ -> Nothing
  where
    getConstructor :: Expression -> Maybe ConstructorRef
    getConstructor f = case f of
      ExpressionIden (IdenConstructor n) -> Just n
      _ -> Nothing
    getVariable :: Expression -> Maybe VarName
    getVariable f = case f of
      ExpressionIden (IdenVar n) -> Just n
      _ -> Nothing

matchInductiveDefs :: InductiveDef -> InductiveDef -> Maybe Text
matchInductiveDefs a b = getError . run . runError . evalState mempty $ go
  where
    getError :: Either Text () -> Maybe Text
    getError = \case
      Left er -> Just er
      Right _ -> Nothing
    compareLengthEq :: [a] -> [b] -> Bool
    compareLengthEq x y = EQ == comparingLength x y
    go :: forall r. r ~ '[State (HashMap Name Name), Error Text] => Sem r ()
    go = do
      addName (a ^. inductiveName) (b ^. inductiveName)
      let paramsA = a ^. inductiveParameters
          paramsB = b ^. inductiveParameters
      unless (compareLengthEq paramsA paramsB) (throw @Text "different number of inductive parameters")
      zipWithM_ matchFunctionParameter paramsA paramsB
      return ()

addName :: Member (State (HashMap Name Name)) r => Name -> Name -> Sem r ()
addName na nb = modify (HashMap.insert na nb)

matchFunctionParameter ::
  forall r.
  Members '[State (HashMap Name Name), Error Text] r =>
  FunctionParameter ->
  FunctionParameter ->
  Sem r ()
matchFunctionParameter pa pb = do
  goParamName (pa ^. paramName) (pb ^. paramName)
  goParamUsage (pa ^. paramUsage) (pb ^. paramUsage)
  goParamImplicit (pa ^. paramImplicit) (pb ^. paramImplicit)
  goParamType (pa ^. paramType) (pb ^. paramType)
  where
    goParamType :: Expression -> Expression -> Sem r ()
    goParamType ua ub = matchExpressions ua ub
    goParamImplicit :: IsImplicit -> IsImplicit -> Sem r ()
    goParamImplicit ua ub = unless (ua == ub) (throw @Text "implicit missmatch")
    goParamUsage :: Usage -> Usage -> Sem r ()
    goParamUsage ua ub = unless (ua == ub) (throw @Text "usage missmatch")
    goParamName :: Maybe VarName -> Maybe VarName -> Sem r ()
    goParamName (Just va) (Just vb) = addName va vb
    goParamName _ _ = return ()

matchExpressions ::
  forall r.
  Members '[State (HashMap Name Name), Error Text] r =>
  Expression ->
  Expression ->
  Sem r ()
matchExpressions = go
  where
    go :: Expression -> Expression -> Sem r ()
    go a b = case (a, b) of
      (ExpressionIden ia, ExpressionIden ib) ->
        unlessM ((== Just (idenName ib)) <$> gets @(HashMap Name Name) (^. at (idenName ia))) err
      (ExpressionIden {}, _) -> err
      (_, ExpressionIden {}) -> err
      (ExpressionApplication ia, ExpressionApplication ib) ->
        goApp ia ib
      (ExpressionApplication {}, _) -> err
      (_, ExpressionApplication {}) -> err
      (ExpressionUniverse ia, ExpressionUniverse ib) ->
        unless (ia == ib) err
      (ExpressionUniverse {}, _) -> err
      (_, ExpressionUniverse {}) -> err
      (ExpressionFunction ia, ExpressionFunction ib) ->
        goFunction ia ib
      (ExpressionFunction {}, _) -> err
      (_, ExpressionFunction {}) -> err
      (ExpressionLiteral ia, ExpressionLiteral ib) ->
        unless (ia == ib) err
      (ExpressionLiteral {}, _) -> err
      (_, ExpressionLiteral {}) -> err
      (ExpressionHole _, ExpressionHole _) -> return ()
    err :: Sem r a
    err = throw @Text "Expression missmatch"
    goApp :: Application -> Application -> Sem r ()
    goApp (Application al ar aim) (Application bl br bim) = do
      unless (aim == bim) err
      go al bl
      go ar br
    goFunction :: Function -> Function -> Sem r ()
    goFunction (Function al ar) (Function bl br) = do
      matchFunctionParameter al bl
      matchExpressions ar br
