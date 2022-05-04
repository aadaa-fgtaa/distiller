module Distiller.Term.Eval where

import Distiller.Name
import Distiller.Term
import Distiller.Term.Print
import Distiller.Prelude
import Data.Map.Strict ( lookup, insert )

-- TODO Proper error handling

data EvalStats = EvalStats { caseReductions :: Int, substitutions :: Int }
  deriving stock (Show, Generic)
  deriving (Semigroup, Monoid) via EvalStats `Wrapping` Sum

tickCaseReduction :: Monad m => ReduceT m ()
tickCaseReduction = tell mempty { caseReductions = 1 }

tickSubstitution :: Monad m => ReduceT m ()
tickSubstitution = tell mempty { substitutions = 1 }


type Env = Map Var Closure

data Closure = Closure Env Term

type ReduceT m = WriterT EvalStats (ReaderT Defs m)

eval :: MonadFresh m => Prog -> m (Term, EvalStats)
eval (Prog main defs) = usingReaderT defs $ runWriterT $ go [] [] main

  where

    go :: MonadFresh m => Env -> [(Env, Context)] -> Term -> ReduceT m Term
    go env ctx = \case

      Var var -> case lookup var env of
        Nothing -> error $ "Unbound variable " <> showPretty var
        Just (Closure env' term) -> go env' ctx term

      Fun fun -> do
        go env ctx =<< askDef fun

      Con con args -> case ctx of
        [] -> do
          tickSubstitution
          Con con <$> traverse (go env []) args
        (env', Match alts):ctx' -> do
          tickCaseReduction
          for_ args $ const tickSubstitution
          case lookup con alts of
            Nothing -> error "Unhandled case"
            Just (Alt pars body) -> go (associate pars (Closure env <$> args) <> env') ctx' body
        (_, Apply _):_ -> error "Constructor appiled"

      Lam par body -> case ctx of
        [] -> pure $ Lam par body
        (env', Apply arg):ctx' -> do
          tickSubstitution
          go (insert par (Closure env' arg) env) ctx' body
        (_, Match _):_ -> error "Lambda scrutineed"

      Let var term body -> do
        tickSubstitution
        go (insert var (Closure env term) env) ctx body

      App fun arg -> do
        go env ((env, Apply arg):ctx) fun

      Case scrut alts -> do
        go env ((env, Match alts):ctx) scrut
