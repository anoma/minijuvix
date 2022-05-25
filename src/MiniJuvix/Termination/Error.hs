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
  = ErrTerminationError FailedTerminationCheck
  deriving stock (Show)

instance ToGenericError TerminationError where
  genericError :: TerminationError -> Maybe GenericError
  genericError = \case
    ErrTerminationError e -> genericError e
