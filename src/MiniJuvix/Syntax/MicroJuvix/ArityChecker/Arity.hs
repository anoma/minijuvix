module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Arity where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

data Arity
  = ArityUnit
  | ArityFunction FunctionArity
  deriving stock (Eq)

data FunctionArity = FunctionArity
  { _functionArityLeft :: ArityParameter,
    _functionArityRight :: Arity
  }
  deriving stock (Eq)

data ArityParameter
  = ParamExplicit Arity
  | ParamImplicit
  deriving stock (Eq)

typeArity :: Type -> Arity
typeArity = go
  where
    go :: Type -> Arity
    go = \case
      TypeIden {} -> ArityUnit
      TypeApp {} -> ArityUnit
      TypeFunction f -> ArityFunction (goFun f)
      TypeAbs f -> ArityFunction (goAbs f)
      TypeHole {} -> ArityUnit
      TypeUniverse {} -> ArityUnit
      TypeAny {} -> ArityUnit
    goFun :: Function -> FunctionArity
    goFun (Function l r) =
      FunctionArity
        { _functionArityLeft = ParamExplicit (go l),
          _functionArityRight = go r
        }
    goAbs :: TypeAbstraction -> FunctionArity
    goAbs t = FunctionArity l r
      where
        r :: Arity
        r = go (t ^. typeAbsBody)
        l :: ArityParameter
        l = case t ^. typeAbsImplicit of
          Implicit -> ParamImplicit
          Explicit -> ParamExplicit ArityUnit

unfoldArity :: Arity -> [ArityParameter]
unfoldArity = go
  where
    go :: Arity -> [ArityParameter]
    go = \case
      ArityUnit -> []
      ArityFunction (FunctionArity l r) -> l : unfoldArity r

foldArity :: [ArityParameter] -> Arity
foldArity = go
  where
  go = \case
    [] -> ArityUnit
    (a : as) -> ArityFunction (FunctionArity l (go as))
      where
      l = case a of
        ParamExplicit e -> ParamExplicit e
        ParamImplicit -> ParamImplicit
