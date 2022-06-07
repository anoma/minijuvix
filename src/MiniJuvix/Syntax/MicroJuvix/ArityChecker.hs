module MiniJuvix.Syntax.MicroJuvix.ArityChecker
  ( module MiniJuvix.Syntax.MicroJuvix.ArityChecker,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult,
    module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error,
  )
where

-- import Data.HashMap.Strict qualified as HashMap

import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Arity
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.LocalVars
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult

entryMicroJuvixArity ::
  Members '[Error ArityCheckerError, NameIdGen] r =>
  MicroJuvixResult ->
  Sem r MicroJuvixArityResult
entryMicroJuvixArity res@MicroJuvixResult {..} = do
  r <- runReader table (mapM checkModule _resultModules)
  return
    MicroJuvixArityResult
      { _resultMicroJuvixResult = res,
        _resultModules = r
      }
  where
    table :: InfoTable
    table = buildTable _resultModules

checkModule ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  let arity = typeArity _funDefType
  _funDefClauses' <- mapM (checkFunctionClause arity) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkFunctionClause ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Arity ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause ari cl = do
  hint <- guessArity (cl ^. clauseBody)
  (patterns', locals, bodyAri) <- checkLhs hint ari (cl ^. clausePatterns)
  body' <- runReader locals (checkExpression (Just bodyAri) (cl ^. clauseBody))
  return
    FunctionClause
      { _clauseName = cl ^. clauseName,
        _clausePatterns = patterns',
        _clauseBody = body'
      }

guessArity :: forall r.
  Members '[Reader InfoTable] r =>
  Expression ->
  Sem r (Maybe Arity)
guessArity = \case
  ExpressionTyped {} -> impossible
  ExpressionHole {} -> return Nothing
  ExpressionFunction {} -> return (Just ArityUnit)
  ExpressionLiteral {} -> return (Just arityLiteral)
  ExpressionApplication a -> appHelper a
  ExpressionIden i -> idenHelper i
  where
  idenHelper :: Iden -> Sem r (Maybe Arity)
  idenHelper i = case i of
    IdenVar {} -> return Nothing
    _ -> Just <$> runReader (LocalVars mempty) (idenArity i)
  appHelper :: Application -> Sem r (Maybe Arity)
  appHelper a = do
    f' <- arif
    return (f' >>= \f'' -> foldArity <$> refine (unfoldArity f'') args)
    where
    refine :: [ArityParameter] -> [IsImplicit] -> Maybe [ArityParameter]
    refine ps as = case (ps, as) of
      (ParamExplicit {} : ps', Explicit : as') -> refine ps' as'
      (ParamImplicit {} : ps', Implicit : as') -> refine ps' as'
      (ParamImplicit {} : ps', as'@(Explicit : _)) -> refine ps' as'
      (ParamExplicit {} : _, Implicit : _) -> Nothing
      (ps', []) -> Just ps'
      ([], _:_) -> Nothing
    (f, args) = second (map fst. toList) (unfoldApplication' a)
    arif :: Sem r (Maybe Arity)
    arif = case f of
      ExpressionHole {} -> return Nothing
      ExpressionApplication {} -> impossible
      ExpressionFunction {} -> return (Just ArityUnit)
      ExpressionLiteral {} -> return (Just arityLiteral)
      ExpressionTyped {} -> impossible
      ExpressionIden i -> idenHelper i

arityLiteral :: Arity
arityLiteral = ArityUnit

checkLhs ::
  forall r.
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Maybe Arity ->
  Arity ->
  [Pattern] ->
  Sem r ([Pattern], LocalVars, Arity)
checkLhs hint ariSignature pats = do
  (locals, (pats', bodyAri)) <- runState emptyLocalVars (goLhs ariSignature pats)
  return (pats', locals, bodyAri)
  where
    -- returns the expanded patterns and the rest of the Arity (the arity of the
    -- body once all the patterns have been processed).
    -- Does not insert holes greedily. I.e. implicit wildcards are only inserted
    -- between explicit parameters already in the pattern.
    goLhs :: Arity -> [Pattern] -> Sem (State LocalVars ': r) ([Pattern], Arity)
    goLhs a = \case
      [] -> return $ case hint >>= tailHelper a of
        Nothing -> ([], a)
        Just tailUnderscores ->
          let a' = foldArity (drop tailUnderscores (unfoldArity a)) in
          (replicate tailUnderscores (PatternBraces PatternWildcard), a')
      lhs@(p : ps) -> case a of
        ArityUnit -> throw @ArityCheckerError (error "too many patterns in Lhs")
        ArityUnknown -> do
          p' <- checkPattern ArityUnknown p
          first (p' : ) <$> goLhs ArityUnknown ps
        ArityFunction (FunctionArity l r) ->
          case (getPatternBraces p, l) of
            (Just b, ParamImplicit) -> first (b :) <$> goLhs r ps
            (Just {}, ParamExplicit {}) -> error "expected an explicit argument"
            (Nothing, ParamImplicit) ->
              first (PatternBraces PatternWildcard :) <$> goLhs r lhs
            (Nothing, ParamExplicit pa) -> do
              p' <- checkPattern pa p
              first (p' :) <$> goLhs r ps

    tailHelper :: Arity -> Arity -> Maybe Int
    tailHelper a target
      | notNull a' && all (== ParamImplicit) a' = Just (length a')
      | otherwise = Nothing
      where
      a' = dropSuffix target' (unfoldArity a)
      target' = unfoldArity target

    getPatternBraces :: Pattern -> Maybe Pattern
    getPatternBraces p = case p of
      PatternBraces {} -> Just p
      _ -> Nothing

checkPattern ::
  Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars] r =>
  Arity ->
  Pattern ->
  Sem r Pattern
checkPattern ari = \case
  PatternBraces {} -> impossible
  PatternVariable v -> addArity v ari $> PatternVariable v
  PatternWildcard -> return PatternWildcard
  PatternConstructorApp c -> case ari of
    ArityUnit -> PatternConstructorApp <$> checkConstructorApp c
    ArityUnknown -> PatternConstructorApp <$> checkConstructorApp c
    ArityFunction {} -> error "Function types cannot be pattern matched"

checkConstructorApp ::
  forall r.
  Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars] r =>
  ConstructorApp ->
  Sem r ConstructorApp
checkConstructorApp (ConstructorApp c ps) = do
  arities <- map typeArity . (^. constructorInfoArgs) <$> lookupConstructor c
  let n = length arities
      lps = length ps
  when (n /= lps) (throw @ArityCheckerError (error "wrong number of args in constructor app"))
  ps' <- zipWithM checkPattern arities ps
  return (ConstructorApp c ps')

idenArity :: Members '[Reader LocalVars, Reader InfoTable] r => Iden -> Sem r Arity
idenArity = \case
  IdenFunction f -> typeArity . (^. functionInfoDef . funDefType) <$> lookupFunction f
  IdenConstructor c -> typeArity <$> constructorType c
  IdenVar v -> getLocalArity v
  IdenAxiom a -> typeArity . (^. axiomInfoType) <$> lookupAxiom a
  IdenInductive i -> typeArity <$> inductiveType i

checkExpression ::
  forall r.
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, Reader LocalVars] r =>
  Maybe Arity ->
  Expression ->
  Sem r Expression
checkExpression hintArity expr = case expr of
  ExpressionIden {} -> appHelper expr []
  ExpressionApplication a -> goApp a
  ExpressionLiteral {} -> return expr
  ExpressionFunction {} -> return expr
  ExpressionHole {} -> return expr
  ExpressionTyped {} -> impossible
  where
    goApp :: Application -> Sem r Expression
    goApp = uncurry appHelper . second toList . unfoldApplication'
    appHelper :: Expression -> [(IsImplicit, Expression)] -> Sem r Expression
    appHelper e args = do
      args' :: [(IsImplicit, Expression)] <- case e of
        ExpressionHole {} -> mapM (secondM (checkExpression Nothing)) args
        ExpressionIden i -> do
          ari <- idenArity i
          let argsAris :: [Arity]
              argsAris = map toArity (unfoldArity ari)
              toArity :: ArityParameter -> Arity
              toArity = \case
                ParamExplicit a -> a
                ParamImplicit -> ArityUnit
          mapM
            (secondM (uncurry checkExpression))
            [(i', (Just a, e')) | (a, (i', e')) <- zip (argsAris ++ repeat ArityUnknown) args]
            >>= addHoles (getLoc i) hintArity ari
        ExpressionLiteral {} -> error "TODO literals on the left of an application"
        ExpressionFunction {} -> throw ErrFutureTypeCheckerError
        ExpressionApplication {} -> impossible
        ExpressionTyped {} -> impossible
      return (foldApplication e args')
    addHoles ::
      Interval ->
      Maybe Arity ->
      Arity ->
      [(IsImplicit, Expression)] ->
      Sem r [(IsImplicit, Expression)]
    addHoles loc hint ari args = case (ari, args) of
      (ArityFunction (FunctionArity ParamImplicit r), (Implicit, e) : rest) ->
        ((Implicit, e) :) <$> addHoles loc hint r rest
      (ArityFunction (FunctionArity (ParamExplicit {}) r), (Explicit, e) : rest) ->
        ((Explicit, e) :) <$> addHoles loc hint r rest
      (ArityFunction (FunctionArity ParamImplicit _), [])
        -- When there are no remaining arguments and the expected arity of the
        -- expression matches the current arity we should *not* insert a hole.
        | Just ari == hint -> return []
      (ArityFunction (FunctionArity ParamImplicit r), _) -> do
        h <- newHole loc
        ((Implicit, ExpressionHole h) :) <$> addHoles loc hint r args
      (ArityFunction (FunctionArity (ParamExplicit {}) _), (Implicit, _) : _) ->
        throw @ArityCheckerError (error "expected explicit but got implicit argument")
      (ArityUnit, []) -> return []
      (ArityFunction (FunctionArity (ParamExplicit _) _), []) -> return []
      (ArityUnit, _ : _) -> error "too many arguments"
      (ArityUnknown, []) -> return []
      (ArityUnknown, p : ps) -> (p :)  <$> addHoles loc hint ArityUnknown ps

newHole :: Member NameIdGen r => Interval -> Sem r Hole
newHole loc = (`Hole` loc) <$> freshNameId
