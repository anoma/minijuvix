module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Arity where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.MicroJuvix.InfoTable

data Arity
  = ArityUnit
  | ArityFunction FunctionArity
  | ArityUnknown
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

typeArity' :: forall r. Members '[Reader InfoTable] r => Type -> Sem r Arity
typeArity' = go
  where
    go :: Type -> Sem r Arity
    go = \case
      TypeIden i -> goIden i
      TypeApp {} -> return ArityUnit
      TypeFunction f -> ArityFunction <$> goFun f
      TypeAbs f -> ArityFunction <$> goAbs f
      TypeHole {} -> return ArityUnknown
      TypeUniverse {} -> return ArityUnit
      TypeAny {} -> return ArityUnknown
    goIden :: TypeIden -> Sem r Arity
    goIden = \case
      TypeIdenVariable {} -> return ArityUnknown
      TypeIdenInductive {} -> return ArityUnit
      TypeIdenAxiom ax -> do
        ty <- (^. axiomInfoType) <$> lookupAxiom ax
        go ty

    goFun :: Function -> Sem r FunctionArity
    goFun (Function l r) = do
      l' <- ParamExplicit <$> go l
      r' <- go r
      return FunctionArity
        { _functionArityLeft = l',
          _functionArityRight = r'
        }
    goAbs :: TypeAbstraction -> Sem r FunctionArity
    goAbs t = do
      r' <- go (t ^. typeAbsBody)
      return (FunctionArity l r')
      where
        l :: ArityParameter
        l = case t ^. typeAbsImplicit of
          Implicit -> ParamImplicit
          Explicit -> ParamExplicit ArityUnit


typeArity :: Type -> Arity
typeArity = go
  where
    go :: Type -> Arity
    go = \case
      TypeIden {} -> ArityUnknown
      TypeApp {} -> ArityUnknown
      TypeFunction f -> ArityFunction (goFun f)
      TypeAbs f -> ArityFunction (goAbs f)
      TypeHole {} -> ArityUnknown
      TypeUniverse {} -> ArityUnit
      TypeAny {} -> ArityUnknown
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
      ArityUnknown -> []
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
