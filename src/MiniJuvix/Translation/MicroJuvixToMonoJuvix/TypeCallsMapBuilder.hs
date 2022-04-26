module MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder (buildTypeCallMap) where

import Data.HashSet qualified as HashSet
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import Data.List.NonEmpty qualified as NonEmpty


buildTypeCallMap :: MicroJuvixTypedResult -> TypeCallsMap
buildTypeCallMap r =
  mkTypeCallsMap
  . fst
  . run
  . runReader (buildTable modules)
  . runOutputMonoid HashSet.singleton
  . mapM_ goModule
  $ modules
  where
    modules = r ^. resultModules

goModule :: Members '[Output TypeCall, Reader InfoTable] r => Module -> Sem r ()
goModule = goModuleBody . (^. moduleBody)

goModuleBody :: Members '[Output TypeCall, Reader InfoTable] r => ModuleBody -> Sem r ()
goModuleBody = mapM_ goStatement . (^. moduleStatements)

goStatement :: Members '[Output TypeCall, Reader InfoTable] r => Statement -> Sem r ()
goStatement = \case
  StatementInductive d -> goInductiveDef d
  StatementFunction f -> goFunctionDef f
  StatementForeign {} -> return ()
  StatementAxiom a -> goAxiomDef a

-- TODO: revise
goAxiomDef :: AxiomDef -> Sem r ()
goAxiomDef _ = return ()

goFunctionDef :: Members '[Output TypeCall, Reader InfoTable] r => FunctionDef -> Sem r ()
goFunctionDef d = runReader (FunctionIden (d ^. funDefName)) $ do
  goType (d ^. funDefType)
  mapM_ goFunctionClause (d ^. funDefClauses)

goFunctionClause :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => FunctionClause -> Sem r ()
goFunctionClause c = goExpression (c ^. clauseBody)

goInductiveDef :: Members '[Output TypeCall, Reader InfoTable] r => InductiveDef -> Sem r ()
goInductiveDef d = runReader (InductiveIden (d ^. inductiveName)) $ do
  mapM_ goInductiveParameter (d ^. inductiveParameters)
  mapM_ goInductiveConstructorDef (d ^. inductiveConstructors)

goInductiveParameter :: InductiveParameter -> Sem r ()
goInductiveParameter _ = return ()

goInductiveConstructorDef :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => InductiveConstructorDef -> Sem r ()
goInductiveConstructorDef c = mapM_ goType (c ^. constructorParameters)

goFunction :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => Function -> Sem r ()
goFunction (Function l r) = do
  goType l
  goType r

goTypeApplication :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => TypeApplication -> Sem r ()
goTypeApplication a = do
  let (t, args) = unfoldTypeApplication a
  mapM_ goType args
  case t of
    TypeIden (TypeIdenInductive n) -> do
      caller <- ask
      output TypeCall' {
        _typeCallCaller = caller,
        _typeCallIden = InductiveIden n,
        _typeCallArguments = args
    }
    _ -> return ()

goTypeAbstraction :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => TypeAbstraction -> Sem r ()
goTypeAbstraction t = goType (t ^. typeAbsBody)

goType :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => Type -> Sem r ()
goType = \case
  TypeIden {} -> return ()
  TypeApp a -> goTypeApplication a
  TypeAny -> return ()
  TypeUniverse -> return ()
  TypeFunction f -> goFunction f
  TypeAbs a -> goTypeAbstraction a

goExpression :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => Expression -> Sem r ()
goExpression = \case
  ExpressionIden {} -> return ()
  ExpressionApplication a -> goApplication a
  ExpressionLiteral {} -> return ()
  ExpressionTyped t -> goExpression (t ^. typedExpression)

expressionAsType' :: Expression -> Type
expressionAsType' = fromMaybe impossible . expressionAsType

goApplication :: Members '[Output TypeCall, Reader TypeAppIden, Reader InfoTable] r => Application -> Sem r ()
goApplication a = do
  let (f, args) = unfoldApplication a
  mapM_ goExpression args
  case f of
    ExpressionIden (IdenFunction fun) -> do
      funTy <- (^. functionInfoDef . funDefType) <$> lookupFunction fun
      let numTyArgs = length (fst (unfoldTypeAbsType funTy))
      when (numTyArgs > 0) $ do
          let tyArgs = fmap expressionAsType' (take' numTyArgs args)
          caller <- ask
          output TypeCall' {
          _typeCallCaller = caller,
          _typeCallIden = FunctionIden fun,
          _typeCallArguments = tyArgs
          }
    _ -> return ()
  where
  take' :: Int -> NonEmpty a -> NonEmpty a
  take' n l
    | 0 < n = fromMaybe impossible . nonEmpty . NonEmpty.take n $ l
    | otherwise = error ("take' non-positive: " <> show n)
