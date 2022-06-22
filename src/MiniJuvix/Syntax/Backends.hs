module MiniJuvix.Syntax.Backends where

import MiniJuvix.Prelude

data Backend = BackendGhc | BackendC
  deriving stock (Show, Eq, Ord, Generic, Lift)

instance Hashable Backend

data BackendItem = BackendItem
  { _backendItemBackend :: Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Ord, Eq, Generic, Lift)

instance Hashable BackendItem

makeLenses ''BackendItem
