module GlobalOptions where

import MiniJuvix.Prelude

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool
  }
makeLenses ''GlobalOptions
