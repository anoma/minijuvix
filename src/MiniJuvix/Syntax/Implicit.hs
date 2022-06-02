module MiniJuvix.Syntax.Implicit where

import MiniJuvix.Prelude

data Implicit = Explicit | Implicit
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Implicit
