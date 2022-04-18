module MiniJuvix.Syntax.MicroJuvix.LocalVars where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude.Pretty

data LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type,
    _localTyMap :: HashMap VarName VarName
  }
  deriving stock (Show)

makeLenses ''LocalVars

instance Pretty LocalVars where
  pretty l =
    "Equalities:" <> line
    <> (vsep (map pretty (toList (l ^. localTyMap))))

addType :: VarName -> Type -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)

emptyLocalVars :: LocalVars
emptyLocalVars = LocalVars {
    _localTypes = mempty,
    _localTyMap = mempty
    }
