module MiniJuvix.Internal.NameIdGen (
  module MiniJuvix.Internal.NameIdGen,
  module MiniJuvix.Syntax.NameId,
 ) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.NameId
import Data.Stream (Stream (Cons))

allNameIds :: Stream NameId
allNameIds = NameId <$> ids
  where
    ids :: Stream Word64
    ids = aux minBound
    aux i = Cons i (aux (succ i))

data NameIdGen m a where
  FreshNameId :: NameIdGen m NameId

makeSem ''NameIdGen

toState :: Sem (NameIdGen ': r) a -> Sem (State (Stream NameId) ': r) a
toState = reinterpret $ \case
  FreshNameId -> do
    (Cons fresh rest) <- get
    put rest
    return fresh

runNameIdGen :: Sem (NameIdGen ': r) a -> Sem r a
runNameIdGen = evalState allNameIds . toState
