module Distiller.Solver where

import Distiller.Name
import Distiller.Prelude
import Data.Map.Strict ( insert, lookup, keysSet )

type Solver m = ReaderT ((Map Var Var, Map Var Var), (Map Fun Fun, Map Fun Fun)) (StateT (Map Var Var) m)

leftBoundVars :: Monad m => Solver m (Set Var)
leftBoundVars = asks $ keysSet . fst . fst

rightBoundVars :: Monad m => Solver m (Set Var)
rightBoundVars = asks $ keysSet . snd . fst

bindVars :: Monad m => Var -> Var -> Solver m a -> Solver m a
bindVars a b = local $ first $ insert a b *** insert b a

bindFuns :: Monad m => Fun -> Fun -> Solver m a -> Solver m a
bindFuns a b = local $ second $ insert a b *** insert b a

bindsVars :: MonadPlus m => [Var] -> [Var] -> Solver m a -> Solver m a
bindsVars a b = if length a == length b
  then local $ first $ mappend (associate a b) *** mappend (associate b a)
  else const empty

bindsFuns :: MonadPlus m => [Fun] -> [Fun] -> Solver m a -> Solver m a
bindsFuns a b = if length a == length b
  then local $ second $ mappend (associate a b) *** mappend (associate b a)
  else const empty

unifyVars :: MonadPlus m => Var -> Var -> Solver m ()
unifyVars a b = do
  ((lr, rl), _) <- ask
  st <- get
  if
    | a == b                 -> pass
    | Just b' <- lookup a lr -> guard (b == b')
    | Just a' <- lookup b rl -> guard (a == a')
    | Just b' <- lookup a st -> guard (b == b')
    | otherwise              -> modify $ insert a b

checkFuns :: MonadPlus m => Fun -> Fun -> Solver m ()
checkFuns a b = do
  (_, (lr, rl)) <- ask
  if
    | a == b                 -> pass
    | Just b' <- lookup a lr -> guard (b == b')
    | Just a' <- lookup b rl -> guard (a == a')
    | otherwise              -> empty

checkCons :: MonadPlus m => Con -> Con -> Solver m ()
checkCons lcon rcon = guard (lcon == rcon)

execSolver :: Monad m => Solver m a -> m (Map Var Var)
execSolver = executingStateT mempty . usingReaderT mempty

evalSolver :: Monad m => Solver m a -> m a
evalSolver = evaluatingStateT mempty . usingReaderT mempty
