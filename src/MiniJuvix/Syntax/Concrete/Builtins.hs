module MiniJuvix.Syntax.Concrete.Builtins where

import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty

data BuiltinInductive
  = BuiltinNatural
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Pretty BuiltinInductive where
  pretty = \case
    BuiltinNatural -> Str.natural

data BuiltinFunction
  = BuiltinNaturalPlus
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Pretty BuiltinFunction where
  pretty = \case
    BuiltinNaturalPlus -> Str.naturalPlus

data BuiltinAxiom
  = BuiltinNaturalPrint
  | BuiltinIO
  | BuiltinIOSequence
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance Pretty BuiltinAxiom where
  pretty = \case
    BuiltinNaturalPrint -> Str.naturalPrint
    BuiltinIO -> Str.io
    BuiltinIOSequence -> Str.ioSequence
