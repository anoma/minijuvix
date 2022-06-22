module MiniJuvix.Syntax.Concrete.Builtins where

import MiniJuvix.Prelude

data BuiltinInductive
  = BuiltinNatural
  deriving stock (Show, Eq, Ord)

data BuiltinFunction
  = BuiltinNaturalPlus
  deriving stock (Show, Eq, Ord)
