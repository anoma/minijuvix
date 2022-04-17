module MiniJuvix.Syntax.MicroJuvix.LocalVars where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude.Pretty

data LocalVars = LocalVars
  { _localTypes :: HashMap VarName Type,
    _localEqualities :: HashSet (VarName, VarName)
  }
  deriving stock (Show)

makeLenses ''LocalVars

instance Pretty LocalVars where
  pretty l =
    "Equalities:" <> line
    <> (vsep (map pretty (toList (l ^. localEqualities))))


addType :: VarName -> Type -> LocalVars -> LocalVars
addType v t = over localTypes (HashMap.insert v t)

addEquality :: VarName -> VarName -> LocalVars -> LocalVars
addEquality a b = over localEqualities (HashSet.union new)
  where
  new = HashSet.fromList [(a, b), (b, a)]

checkEqual :: VarName -> VarName -> LocalVars -> Bool
checkEqual a b l = HashSet.member (a, b) (l ^. localEqualities)

-- instance Semigroup LocalVars where
--   a <> b = LocalVars {
--     _localTypes = a ^. localTypes <> b ^. localTypes,
--     -- TODO revise this.
--     _localEqualities = a ^. localEqualities <> b ^. localEqualities
--     }
emptyLocalVars :: LocalVars
emptyLocalVars = LocalVars {
    _localTypes = mempty,
    _localEqualities = mempty
    }

-- instance Monoid LocalVars where
--   mempty = LocalVars {
--     _localTypes = mempty,
--     _localEqualities = mempty
--     }
