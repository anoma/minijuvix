module MiniJuvix.Builtins.Effect
  ( module MiniJuvix.Builtins.Effect,
    module MiniJuvix.Builtins.Base,
  )
where

import MiniJuvix.Builtins.Base
import MiniJuvix.Builtins.Error
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra

data Builtins m a where
  GetBuiltin :: Interval -> BuiltinsEnum -> Builtins m Name
  RegisterBuiltin :: BuiltinsEnum -> Name -> Builtins m ()

makeSem ''Builtins

newtype BuiltinsState = BuiltinsState
  { _builtinsTable :: HashMap BuiltinsEnum Name
  }

makeLenses ''BuiltinsState

iniState :: BuiltinsState
iniState = BuiltinsState mempty

re :: forall r a. Member (Error MiniJuvixError) r => Sem (Builtins ': r) a -> Sem (State BuiltinsState ': r) a
re = reinterpret $ \case
  GetBuiltin i b -> fromMaybeM notDefined (gets (^. builtinsTable . at b))
    where
      notDefined :: Sem (State BuiltinsState : r) x
      notDefined =
        throw $
          MiniJuvixError
            NotDefined
              { _notDefinedBuiltin = b,
                _notDefinedLoc = i
              }
  RegisterBuiltin b n -> do
    s <- gets (^. builtinsTable . at b)
    case s of
      Nothing -> modify (over builtinsTable (set (at b) (Just n)))
      Just {} -> alreadyDefined
    where
      alreadyDefined :: Sem (State BuiltinsState : r) x
      alreadyDefined =
        throw $
          MiniJuvixError
            AlreadyDefined
              { _alreadyDefinedBuiltin = b,
                _alreadyDefinedLoc = getLoc n
              }

runBuiltins :: Member (Error MiniJuvixError) r => Sem (Builtins ': r) a -> Sem r a
runBuiltins = evalState iniState . re
