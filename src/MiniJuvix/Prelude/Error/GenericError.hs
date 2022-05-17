module MiniJuvix.Prelude.Error.GenericError where

import MiniJuvix.Prelude.Base
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Loc
import Prettyprinter.Render.Text

data GenericError = GenericError
  { _genericErrorLoc :: Loc,
    _genericErrorFile :: FilePath,
    _genericErrorMessage :: Text,
    _genericErrorIntervals :: [Interval]
  }
  deriving stock (Show)

makeLenses ''GenericError

class ToGenericError a where
  genericError :: a -> Maybe GenericError

instance ToGenericError Text where
  genericError = const Nothing

instance Pretty GenericError where
  pretty :: GenericError -> Doc a
  pretty g =
    let lineNum = g ^. genericErrorLoc . locFileLoc . locLine
        colNum = g ^. genericErrorLoc . locFileLoc . locCol
     in pretty (g ^. genericErrorFile)
          <> colon
          <> pretty lineNum
          <> colon
          <> pretty colNum
          <> colon <+> "error"
          <> colon <+> pretty (g ^. genericErrorMessage)
          <> line

errorIntervals :: ToGenericError e => e -> [Interval]
errorIntervals = maybe [] (^. genericErrorIntervals) . genericError

renderGenericError :: GenericError -> Text
renderGenericError = renderStrict . layoutPretty defaultLayoutOptions . pretty
