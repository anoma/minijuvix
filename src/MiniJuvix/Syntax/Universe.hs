module MiniJuvix.Syntax.Universe where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Fixity

data Universe = Universe
  { _universeLevel :: Maybe Natural,
    _universeLoc :: Interval
  }
  deriving stock (Show, Ord)

newtype SmallUniverse = SmallUniverse
  { _smallUniverseLoc :: Interval
  }
  deriving stock (Show)

instance Eq Universe where
  (Universe a _) == (Universe b _) = f a == f b
    where
      f :: Maybe Natural -> Natural
      f = fromMaybe defaultLevel

defaultLevel :: Natural
defaultLevel = 0

smallLevel :: Natural
smallLevel = 0

makeLenses ''Universe
makeLenses ''SmallUniverse

smallUniverse :: Interval -> Universe
smallUniverse = Universe (Just smallLevel)

isSmallUniverse :: Universe -> Bool
isSmallUniverse = (== smallLevel) . fromMaybe defaultLevel . (^. universeLevel)

instance HasAtomicity Universe where
  atomicity u = case u ^. universeLevel of
    Nothing -> Atom
    Just {} -> Aggregate appFixity

instance HasAtomicity SmallUniverse where
  atomicity _ = Atom

instance HasLoc Universe where
  getLoc = (^. universeLoc)

instance HasLoc SmallUniverse where
  getLoc = (^. smallUniverseLoc)
