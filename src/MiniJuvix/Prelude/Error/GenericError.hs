module MiniJuvix.Prelude.Error.GenericError where

import MiniJuvix.Prelude.Base
import MiniJuvix.Syntax.Concrete.Loc

data GenericError = GenericError {
  _genericErrorLoc :: Loc,
  _genericErrorFile :: FilePath,
  _genericErrorMessage :: Text,
  _genericErrorIntervals :: [Interval]
  }
makeLenses ''GenericError

class ToGenericError a where
  genericError :: a -> Maybe GenericError

instance ToGenericError Text where
  genericError = const Nothing
