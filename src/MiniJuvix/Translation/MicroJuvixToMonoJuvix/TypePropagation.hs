module MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypePropagation (collectTypeCalls) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import Data.HashSet qualified as HashSet
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder

collectTypeCalls :: MicroJuvixTypedResult -> TypeCalls
collectTypeCalls res = run (execState emptyCalls (runReader typesTable (runReader infoTable goTopLevel)))
  where
  goTopLevel :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r => Sem r ()
  goTopLevel = mapM_ goConcreteFun entries
    where
    -- | the list of functions defined in the Main module with concrete types.
    entries :: [FunctionDef]
    entries = [ f | StatementFunction f <- main ^. moduleBody . moduleStatements,
              hasConcreteType f ]
      where
        hasConcreteType :: FunctionDef -> Bool
        hasConcreteType = isJust . mkConcreteType . (^. funDefType)
    goConcreteFun :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r => FunctionDef -> Sem r ()
    goConcreteFun fun = do
      calls <- fmap (fmap mkConcreteType') <$> lookupTypeCalls funIden
      mapM_ go calls
      where
      funIden :: TypeCallIden
      funIden = FunctionIden (fun ^. funDefName)
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

lookupTypeCalls :: Members '[Reader TypeCallsMap] r => TypeCallIden -> Sem r [TypeCall]
lookupTypeCalls t = fromMaybe [] . fmap toList <$> asks (^. typeCallsMap . at t)

toConcreteCall :: HashMap VarName ConcreteType -> TypeCall -> ConcreteTypeCall
toConcreteCall m = fmap (substitutionConcrete m)

go :: Members '[State TypeCalls, Reader TypeCallsMap, Reader InfoTable] r
  => ConcreteTypeCall -> Sem r ()
go c = unlessM (isRegistered c) $ do
  register c
  calls :: [TypeCall] <- lookupTypeCalls (c ^. typeCallIden)
  assocs :: HashMap VarName ConcreteType <- case c ^. typeCallIden of
    InductiveIden i -> do
      def <- (^. inductiveInfoDef) <$> lookupInductive i
      return (inductiveTypeVarsAssoc def (c ^. typeCallArguments))
    FunctionIden f -> do
      def <- (^. functionInfoDef) <$> lookupFunction f
      return (functionTypeVarsAssoc def (c ^. typeCallArguments))
  let ccalls :: [ConcreteTypeCall]
      ccalls = fmap (toConcreteCall assocs) calls
  mapM_ go ccalls
