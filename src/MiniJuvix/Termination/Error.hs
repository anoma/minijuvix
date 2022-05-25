module MiniJuvix.Termination.Error
  (module MiniJuvix.Termination.Error,
   module MiniJuvix.Termination.Error.Pretty,
   module MiniJuvix.Termination.Error.Types
  )
where

import MiniJuvix.Termination.Error.Pretty
import MiniJuvix.Termination.Error.Types
import MiniJuvix.Prelude

newtype TerminationError
  = ErrNoLexOrder NoLexOrder
  deriving stock (Show)

instance ToGenericError TerminationError where
  genericError :: TerminationError -> Maybe GenericError
  genericError = \case
    ErrNoLexOrder e -> genericError e
