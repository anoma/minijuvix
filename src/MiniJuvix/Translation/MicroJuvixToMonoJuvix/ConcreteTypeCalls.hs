module MiniJuvix.Translation.MicroJuvixToMonoJuvix.ConcreteTypeCalls where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import Data.HashSet qualified as HashSet
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import MiniJuvix.Translation.MicroJuvixToMonoJuvix.TypeCallsMapBuilder

newtype TypeCalls = TypeCalls {
  _typeCallSet :: HashSet ConcreteTypeCall
   }

makeLenses ''TypeCalls

collectTypeCalls :: MicroJuvixTypedResult -> TypeCalls
collectTypeCalls res = undefined
  where
  mainFunction :: FunctionDef
  mainFunction = fromMaybe (error "no main function found")
            (find isMainFun [ fun |  StatementFunction fun <- main ^. moduleBody . moduleStatements ])
    where
    isMainFun :: FunctionDef -> Bool
    isMainFun = ("main" == ) . (^. funDefName . nameText)
  main :: Module
  main = res ^. mainModule
  table :: TypeCallsMap
  table = buildTypeCallMap res

isRegistered :: Members '[State TypeCalls] r => ConcreteTypeCall -> Sem r Bool
isRegistered c = gets (HashSet.member c . (^. typeCallSet))

register :: Members '[State TypeCalls] r => ConcreteTypeCall -> Sem r ()
register c = modify (over typeCallSet (HashSet.insert c))

lookupTypeCalls :: Members '[Reader TypeCallsMap] r => TypeAppIden -> Sem r (NonEmpty TypeCall)
lookupTypeCalls t = fromJust <$> asks (^. typeCallsMap . at t)

go :: Members '[State TypeCalls, Reader TypeCallsMap] r => ConcreteTypeCall -> Sem r ()
go c = unlessM (isRegistered c) $ do
  register c
  calls <- lookupTypeCalls (c ^. typeCallCaller)
  undefined
