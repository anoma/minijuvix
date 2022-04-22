module MiniJuvix.Syntax.Backends where

import MiniJuvix.Prelude

data Backend = BackendGhc | BackendAgda
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Backend

data BackendItem = BackendItem
  { _backendItemBackend :: Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Ord)

instance Eq BackendItem where
  (==) = (==) `on` _backendItemBackend

instance Hashable BackendItem where
  hashWithSalt b = hashWithSalt b . _backendItemBackend

makeLenses ''BackendItem
