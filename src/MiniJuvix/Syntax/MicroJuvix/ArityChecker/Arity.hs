module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Arity where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

data Arity
  = ArityUnit
  | ArityFunction FunctionArity

data FunctionArity = FunctionArity
  { _functionArityLeft :: ArityParameter,
    _functionArityRight :: Arity
  }

data ArityParameter
  = ParamExplicit Arity
  | ParamImplicit

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
