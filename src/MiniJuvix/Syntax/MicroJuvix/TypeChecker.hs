module MiniJuvix.Syntax.MicroJuvix.TypeChecker
  ( module MiniJuvix.Syntax.MicroJuvix.TypeChecker,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module MiniJuvix.Syntax.MicroJuvix.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.Concrete.Language (LiteralLoc)
import MiniJuvix.Syntax.MicroJuvix.Error
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.LocalVars
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import Polysemy.Error (fromEither)

entryMicroJuvixTyped ::
  (Member (Error TypeCheckerErrors) r) =>
  MicroJuvixResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixResult {..} = do
  r <- fromEither (mapM checkModule _resultModules)
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixResult = res,
        _resultModules = r
      }

checkModule :: Module -> Either TypeCheckerErrors Module
checkModule m = run $ do
  (es, checkedModule) <- runOutputList $ runReader (buildTable m) (checkModule' m)
  return $ case nonEmpty es of
    Nothing -> Right checkedModule
    Just xs -> Left (TypeCheckerErrors {_unTypeCheckerErrors = xs})

checkModule' ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  Module ->
  Sem r Module
checkModule' Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkStatement ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  info <- lookupFunction _funDefName
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars] r =>
  Type ->
  Expression ->
  Sem r Expression
checkExpression t e = do
  t' <- inferExpression' e
  let inferredType = t' ^. typedType
  unlessM (matchTypes t inferredType) (throw (err inferredType))
  return (ExpressionTyped t')
  where
    err infTy =
      ErrWrongType
        ( WrongType
            { _wrongTypeExpression = e,
              _wrongTypeInferredType = infTy,
              _wrongTypeExpectedType = t
            }
        )

matchTypes ::
  Members '[Reader InfoTable] r =>
  Type ->
  Type ->
  Sem r Bool
matchTypes a b = do
  return $
    a == TypeAny || b == TypeAny || alphaEq a b

normalizeType :: Members '[Reader InfoTable] r => Type -> Sem r Type
normalizeType = return . run . runReader ini . goType
  where
  ini :: HashMap VarName Type
  ini = mempty
  goType :: forall r. Member (Reader (HashMap VarName Type)) r => Type -> Sem r Type
  goType t = case t of
    TypeAny -> return TypeAny
    TypeUniverse -> return TypeUniverse
    TypeFunction f -> TypeFunction <$> goFunction f
    TypeIden i -> goIden i
    TypeAbs i -> TypeAbs <$> goAbs i
    TypeApp a -> goApp a
    where
    goIden :: TypeIden -> Sem r Type
    goIden i = case i of
      TypeIdenInductive {} -> return (TypeIden i)
      TypeIdenAxiom {} -> return (TypeIden i)
      TypeIdenVariable v -> do
        res <- HashMap.lookup v <$> ask
        return $ case res of
          Just ty -> ty
          Nothing -> TypeIden i
    goFunction :: Function -> Sem r Function
    goFunction (Function l r) = do
      l' <- goType l
      r' <- goType r
      return (Function l' r')
    goApp :: TypeApplication -> Sem r Type
    goApp (TypeApplication l r) = do
      l' <- goType l
      r' <- goType r
      case l' of
        TypeAbs (TypeAbstraction v body) -> do
          local (HashMap.insert v r') (goType body)
        _ -> return (TypeApp (TypeApplication l' r'))
    goAbs :: TypeAbstraction -> Sem r TypeAbstraction
    goAbs (TypeAbstraction v r) = do
      r' <- goType r
      return TypeAbstraction {
        _typeAbsVar = v,
        _typeAbsBody= r'
      }


-- | Alpha equivalence
alphaEq :: Type -> Type -> Bool
alphaEq ty = run . runReader ini . go ty
 where
 ini :: HashMap VarName VarName
 ini = mempty
 go :: forall r. Member (Reader (HashMap VarName VarName)) r => Type -> Type -> Sem r Bool
 go a' b' = case (a', b') of
  (TypeIden a, TypeIden b) -> goIden a b
  (TypeApp a, TypeApp b) -> goApp a b
  (TypeAbs a, TypeAbs b) -> goAbs a b
  (TypeFunction a, TypeFunction b) -> goFunction a b
  (TypeUniverse, TypeUniverse) -> return True
  (TypeAny, TypeAny) -> return True
  -- TODO is the final wildcard bad style?
  -- what if more Type constructors are added
  _ -> return False
  where
  goIden :: TypeIden -> TypeIden -> Sem r Bool
  goIden ia ib = case (ia, ib) of
    (TypeIdenInductive a, TypeIdenInductive b) -> return (a == b)
    (TypeIdenAxiom a, TypeIdenAxiom b) -> return (a == b)
    (TypeIdenVariable a, TypeIdenVariable b) -> do
      la <- fromMaybe False . fmap (== b) . HashMap.lookup a <$> ask
      return (a == b || la)
    _ -> return False
  goApp :: TypeApplication -> TypeApplication -> Sem r Bool
  goApp (TypeApplication f x) (TypeApplication f' x') = andM [go f f', go x x']
  goFunction :: Function -> Function -> Sem r Bool
  goFunction (Function l r) (Function l' r') = andM [go l r, go l' r']
  goAbs :: TypeAbstraction -> TypeAbstraction -> Sem r Bool
  goAbs (TypeAbstraction v1 r) (TypeAbstraction v2 r') =
    local (HashMap.insert v1 v2) (go r r')

inferExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, Reader LocalVars] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap ExpressionTyped . inferExpression'

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor f = HashMap.lookupDefault impossible f <$> asks _infoConstructors

lookupInductive :: Member (Reader InfoTable) r => InductiveName -> Sem r InductiveInfo
lookupInductive f = HashMap.lookupDefault impossible f <$> asks _infoInductives

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction f = HashMap.lookupDefault impossible f <$> asks _infoFunctions

lookupAxiom :: Member (Reader InfoTable) r => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks _infoAxioms

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Type
lookupVar v = HashMap.lookupDefault impossible v <$> asks _localTypes

constructorType :: Member (Reader InfoTable) r => Name -> Sem r Type
constructorType c = do
  info <- lookupConstructor c
  let r = TypeIden (TypeIdenInductive (info ^. constructorInfoInductive))
  return (foldFunType (args info) r)
  where
  args info = map FunctionArgTypeAbstraction as
   ++ map FunctionArgTypeType bs
    where (as, bs) = constructorArgTypes info

constructorArgTypes :: ConstructorInfo -> ([VarName],[Type])
constructorArgTypes i =
  (map goParam (i ^. constructorInfoInductiveParameters)
  , i ^. constructorInfoArgs)
  where
  goParam = (^. inductiveParamName)

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [FunctionArgType] -> Type -> Type
foldFunType l r = case l of
  [] -> r
  (a : as) ->
    let r' = foldFunType as r in
    case a of
      FunctionArgTypeAbstraction v -> TypeAbs (TypeAbstraction v r')
      FunctionArgTypeType t -> TypeFunction (Function t r')

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Type -> ([FunctionArgType], Type)
unfoldFunType t = case t of
  TypeFunction (Function l r) -> first (FunctionArgTypeType l :) (unfoldFunType r)
  TypeAbs (TypeAbstraction var r) -> first (FunctionArgTypeAbstraction var :) (unfoldFunType r)
  _ -> ([], t)

checkFunctionClause ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info clause@FunctionClause {..} = do
  let (argTys, rty) = unfoldFunType (info ^. functionInfoType)
      (patTys, restTys) = splitAt (length _clausePatterns) argTys
      bodyTy = foldFunType restTys rty
  if
    | length patTys /= length _clausePatterns -> output (tyErr patTys) $> clause
    | otherwise -> do
      eLocals <- checkPatterns _clauseName patTys _clausePatterns
      _clauseBody' <- case eLocals of
        Left err -> output err $> _clauseBody
        Right locals -> do
          eclauseBody <- runError @TypeCheckerError $ runReader locals (checkExpression bodyTy _clauseBody)
          case eclauseBody of
            Left err -> output err $> _clauseBody
            Right r -> return r
      return
        FunctionClause
          { _clauseBody = _clauseBody',
            ..
          }
  where
    tyErr :: [FunctionArgType] -> TypeCheckerError
    tyErr patTys =
      ErrTooManyPatterns
        ( TooManyPatterns
            { _tooManyPatternsClause = clause,
              _tooManyPatternsTypes = patTys
            }
        )

checkPatterns ::
  Members '[Reader InfoTable, Output TypeCheckerError] r =>
  FunctionName ->
  [FunctionArgType] ->
  [Pattern] ->
  Sem r (Either TypeCheckerError LocalVars)
checkPatterns name ctorTys ctorPs =
  runError @TypeCheckerError (mconcat <$> zipWithM (checkPattern name) ctorTys ctorPs)

typeOfArg :: FunctionArgType -> Type
typeOfArg a = case a of
  FunctionArgTypeAbstraction {} -> TypeUniverse
  FunctionArgTypeType ty -> ty

substitution :: [(InductiveParameter, Type)] -> Type -> Type
substitution as = go
  where
  go :: Type -> Type
  go = \case
    TypeIden i -> goIden i
    TypeApp a -> TypeApp (goApp a)
    TypeAbs a -> TypeAbs (goAbs a)
    TypeFunction f -> TypeFunction (goFunction f)
    TypeUniverse -> TypeUniverse
    TypeAny -> TypeAny
  goApp :: TypeApplication -> TypeApplication
  goApp (TypeApplication l r) = TypeApplication (go l) (go r)
  goAbs :: TypeAbstraction -> TypeAbstraction
  goAbs (TypeAbstraction v b) = (TypeAbstraction v (go b))
  goFunction :: Function -> Function
  goFunction (Function l r) = Function (go l) (go r)
  goIden :: TypeIden -> Type
  goIden i = case i of
    TypeIdenInductive {} -> TypeIden i
    TypeIdenAxiom {} -> TypeIden i
    TypeIdenVariable v -> case HashMap.lookup v m of
      Just ty -> ty
      Nothing -> TypeIden i
  m = HashMap.fromList (map (first (^. inductiveParamName)) as)

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Output TypeCheckerError, Error TypeCheckerError] r =>
  FunctionName ->
  FunctionArgType ->
  Pattern ->
  Sem r LocalVars
checkPattern funName type_ pat = LocalVars . HashMap.fromList <$> go type_ pat
  where
    checkSaturatedInductive :: Type -> Sem r (InductiveName, [(InductiveParameter, Type)])
    checkSaturatedInductive t = do
      (ind, args) <- viewInductiveApp t
      params <- (^. inductiveInfoDef . inductiveParameters)
          <$> lookupInductive ind
      let numArgs = length args
          numParams = length params
      when (numArgs < numParams) (error "unsaturated inductive type")
      when (numArgs > numParams) (error "too many arguments to inductive type")
      return (ind, zip params args)
    go :: FunctionArgType -> Pattern -> Sem r [(VarName, Type)]
    go argTy p = let ty = typeOfArg argTy in
      case p of
      PatternWildcard -> return []
      PatternVariable v -> return [(v, ty)]
      PatternConstructorApp a -> do
        (ind, tyArgs) <- checkSaturatedInductive ty
        info <- lookupConstructor (a ^. constrAppConstructor)
        let constrInd = info ^. constructorInfoInductive
        when (ind /= constrInd) (throw (ErrWrongConstructorType
                 (WrongConstructorType (a ^. constrAppConstructor) ind constrInd funName)))
        goConstr a tyArgs
      where
        goConstr :: ConstructorApp -> [(InductiveParameter, Type)] -> Sem r [(VarName, Type)]
        goConstr app@(ConstructorApp c ps) ctx = do
          (tyvars, psTys) <- constructorArgTypes <$> lookupConstructor c
          let psTys' = map (substitution ctx) psTys
              expectedNum = length tyvars + length psTys
          let w = map FunctionArgTypeAbstraction tyvars
                  ++ map FunctionArgTypeType psTys'
          when (expectedNum /= length ps) (throw (appErr app w))
          concat <$> zipWithM go w ps
        appErr :: ConstructorApp -> [FunctionArgType] -> TypeCheckerError
        appErr app tys =
          ErrWrongConstructorAppArgs
            (WrongConstructorAppArgs
                { _wrongCtorAppApp = app,
                  _wrongCtorAppTypes = tys,
                  _wrongCtorAppName = funName
                }
            )

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionTyped t -> return t
  ExpressionLiteral l -> goLiteral l
  where
    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral l = return (TypedExpression TypeAny (ExpressionLiteral l))
    inferIden :: Iden -> Sem r TypedExpression
    inferIden i = case i of
      IdenFunction fun -> do
        info <- lookupFunction fun
        return (TypedExpression (info ^. functionInfoType) (ExpressionIden i))
      IdenConstructor c -> do
        ty <- constructorType c
        return (TypedExpression ty (ExpressionIden i))
      IdenVar v -> do
        ty <- lookupVar v
        return (TypedExpression ty (ExpressionIden i))
      IdenAxiom v -> do
        info <- lookupAxiom v
        return (TypedExpression (info ^. axiomInfoType) (ExpressionIden i))
    inferApplication :: Application -> Sem r TypedExpression
    inferApplication a = do
      let leftExp = a ^. appLeft
      l <- inferExpression' leftExp
      fun <- getFunctionType leftExp (l ^. typedType)
      r <- checkExpression (fun ^. funLeft) (a ^. appRight)
      return
        TypedExpression
          { _typedExpression =
              ExpressionApplication
                Application
                  { _appLeft = ExpressionTyped l,
                    _appRight = r
                  },
            _typedType = fun ^. funRight
          }
      where
        getFunctionType :: Expression -> Type -> Sem r Function
        getFunctionType appExp t = case t of
          TypeFunction f -> return f
          _ -> throw tyErr
          where
            tyErr :: TypeCheckerError
            tyErr =
              ErrExpectedFunctionType
                ( ExpectedFunctionType
                    { _expectedFunctionTypeExpression = e,
                      _expectedFunctionTypeApp = appExp,
                      _expectedFunctionTypeType = t
                    }
                )

viewInductiveApp :: Member (Error TypeCheckerError) r =>
   Type -> Sem r (InductiveName, [Type])
viewInductiveApp ty = case t of
  TypeIden (TypeIdenInductive n) -> return (n, as)
  _ -> error "only inductive types can be pattern matched"
  where
  (t, as) = viewTypeApp ty

viewTypeApp :: Type -> (Type, [Type])
viewTypeApp t = case t of
  TypeApp (TypeApplication l r) ->
    second (`snoc` r) (viewTypeApp l)
  TypeAny {} -> (t, [])
  TypeUniverse {} -> (t, [])
  TypeAbs {} -> (t, [])
  TypeFunction {} -> (t, [])
  TypeIden {} -> (t, [])
