module MiniJuvix.Syntax.Hole where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.NameId
import Prettyprinter

data Hole = Hole
  { _holeId :: NameId,
    _holeLoc :: Interval
  }
  deriving stock (Show, Eq, Ord)

makeLenses ''Hole

instance Hashable Hole where
  hashWithSalt s = hashWithSalt s . (^. holeId)

instance HasLoc Hole where
  getLoc = (^. holeLoc)

instance Pretty Hole where
  pretty = const "_"
