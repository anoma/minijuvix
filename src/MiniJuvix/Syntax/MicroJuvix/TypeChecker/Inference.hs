module MiniJuvix.Syntax.MicroJuvix.TypeChecker.Inference where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.Error

data MetavarState
  = Fresh
  | -- | MergedWith Hole
    Solved Type

data Inference m a where
  FreshMetavar :: Hole -> Inference m TypedExpression
  QueryMetavar :: Hole -> Inference m (Maybe Type)
  -- MergeMetavars :: Hole -> Hole -> Inference m ()
  -- QueryMetavar :: Hole -> Inference m MetavarState
  SolveMetavar :: Hole -> Type -> Inference m ()

makeSem ''Inference

newtype InferenceState = InferenceState
  { _inferenceMap :: HashMap Hole MetavarState
  }

makeLenses ''InferenceState

iniState :: InferenceState
iniState = InferenceState mempty

closeState :: Member (Error TypeCheckerError) r => InferenceState -> Sem r (HashMap Hole Type)
closeState = \case
  InferenceState m ->
    case zip (HashMap.keys m) <$> mapM (uncurry getSolved) (HashMap.toList m) of
      Left {} -> throw @TypeCheckerError (error "unsolved meta")
      Right r -> return (HashMap.fromList r)
  where
    getSolved :: Hole -> MetavarState -> Either Hole Type
    getSolved h = \case
      Solved t -> return t
      Fresh -> Left h

getMetavar :: Member (State InferenceState) r => Hole -> Sem r MetavarState
getMetavar h = gets (fromJust . (^. inferenceMap . at h))

re :: Sem (Inference ': r) Expression -> Sem (State InferenceState ': r) Expression
re = reinterpret $ \case
  FreshMetavar h -> do
    modify (over inferenceMap (HashMap.insert h Fresh))
    return TypedExpression {
      _typedExpression = ExpressionHole h,
      _typedType = TypeUniverse
      }
  QueryMetavar h -> getSolved <$> getMetavar h
  SolveMetavar h t
   | hasHoles t -> error "unsupported: t has holes"
   | otherwise -> do
      s <- gets (fromJust . (^. inferenceMap . at h))
      case s of
        Fresh -> modify (over inferenceMap (HashMap.insert h (Solved t)))
        Solved {} -> error "bug: already solved"
  where
  getSolved :: MetavarState -> Maybe Type
  getSolved = \case
    Fresh -> Nothing
    Solved t -> Just t

runInference :: Member (Error TypeCheckerError) r => Sem (Inference ': r) Expression -> Sem r Expression
runInference a = do
  (subs, expr) <- runState iniState (re a) >>= firstM closeState
  return (fillHoles subs expr)
