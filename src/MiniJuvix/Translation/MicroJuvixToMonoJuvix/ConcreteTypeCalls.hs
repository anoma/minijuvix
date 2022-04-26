module MiniJuvix.Translation.MicroJuvixToMonoJuvix.ConcreteTypeCalls (collectTypeCalls) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import Data.HashSet qualified as HashSet
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder

collectTypeCalls :: MicroJuvixTypedResult -> TypeCalls
collectTypeCalls res = run (execState emptyCalls (runReader typesTable (runReader infoTable goMain)))
  where
  goMain :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r => Sem r ()
  goMain = do
    calls <- fmap (fmap mkConcreteType') <$> lookupTypeCalls mainIden
    mapM_ go calls
    where
    mainIden :: TypeAppIden
    mainIden = FunctionIden (mainFunction ^. funDefName)
  mainFunction :: FunctionDef
  mainFunction = fromMaybe (error "no main function found")
            (find isMainFun [ fun |  StatementFunction fun <- main ^. moduleBody . moduleStatements ])
    where
    isMainFun :: FunctionDef -> Bool
    isMainFun = ("main" == ) . (^. funDefName . nameText)
  main :: Module
  main = res ^. mainModule
  typesTable :: TypeCallsMap
  typesTable = buildTypeCallMap res
  infoTable :: InfoTable
  infoTable = buildTable (res ^. resultModules)

isRegistered :: Members '[State TypeCalls] r => ConcreteTypeCall -> Sem r Bool
isRegistered c = gets (HashSet.member c . (^. typeCallSet))

register :: Members '[State TypeCalls] r => ConcreteTypeCall -> Sem r ()
register c = modify (over typeCallSet (HashSet.insert c))

lookupTypeCalls :: Members '[Reader TypeCallsMap] r => TypeAppIden -> Sem r (NonEmpty TypeCall)
lookupTypeCalls t = fromJust <$> asks (^. typeCallsMap . at t)

toConcreteCall :: HashMap VarName ConcreteType -> TypeCall -> ConcreteTypeCall
toConcreteCall m = fmap (substitutionConcrete m)

go :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r
  => ConcreteTypeCall -> Sem r ()
go c = unlessM (isRegistered c) $ do
  register c
  calls :: NonEmpty TypeCall <- lookupTypeCalls (c ^. typeCallCaller)
  assocs :: HashMap VarName ConcreteType <- case c ^. typeCallCaller of
    InductiveIden i -> do
      def <- (^. inductiveInfoDef) <$> lookupInductive i
      return (inductiveTypeVarsAssoc def (c ^. typeCallArguments))
    FunctionIden f -> do
      def <- (^. functionInfoDef) <$> lookupFunction f
      return (functionTypeVarsAssoc def (c ^. typeCallArguments))
  let ccalls :: NonEmpty ConcreteTypeCall
      ccalls = fmap (toConcreteCall assocs) calls
  mapM_ go ccalls
