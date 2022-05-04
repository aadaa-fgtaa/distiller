module Distiller.Transform where

import Distiller.Name
import Distiller.Tree as Tree
import Distiller.Term as Term
import Distiller.Prelude
import Distiller.Residualise
import Data.Set ( member, insert )
import Data.Foldable.Extra ( firstJustM )

-- TODO Proper error handling

transformProg :: MonadFresh m => Int -> Prog -> m Prog
transformProg n (Prog main defs) = residualise =<< usingReaderT defs do transform n [] main

distillProg :: MonadFresh m => Prog -> m Prog
distillProg (Prog main defs) = residualise =<< usingReaderT defs do distill 0 main


transform :: MonadFresh m => Int -> [Context] -> Term -> ReaderT Defs m Tree

transform n ctx0 term0 | n <= 0 = go [] (place ctx0 term0)
  where
    go rho = \case
      Term.Var var -> do
        pure $ Tree.Var var

      Term.Fun fun -> if fun `member` rho
        then pure $ Tree.Use fun []
        else do
          res <- go (insert fun rho) =<< askDef fun
          pure $ mkDef fun [] res

      Term.Con con args -> do
        Tree.Con con <$> traverse (go rho) args

      Term.App fun arg -> do
        Tree.App <$> go rho fun <*> go rho arg

      Term.Lam par body -> do
        Tree.Lam par <$> go rho body

      Term.Let var term body -> do
        Tree.Let var <$> go rho term <*> go rho body

      Term.Case scrut alts -> do
        Tree.Case <$> go rho scrut <*> for alts \(Term.Alt pars body) -> Tree.Alt pars <$> go rho body

transform n ctx0 term0 = go [] ctx0 term0

  where

    go rho ctx = \case

      Term.Var var -> do
        go' rho ctx (Tree.Var var)

      Term.Con con args -> case ctx of
        []              -> Tree.Con con <$> traverse (go rho []) args
        Match alts:ctx' -> go rho ctx' =<< reduceCase con args alts
        Apply _   :_    -> error "Constructor applied"

      Term.Lam par body -> case ctx of
        []             -> Tree.Lam par <$> go rho [] body
        Apply arg:ctx' -> go rho ctx' =<< reduceLam par arg body
        Match _  :_    -> error "Lambda scrutineed"

      Term.App fun arg -> go rho (Apply arg:ctx) fun

      Term.Case scrut alts -> go rho (Match alts:ctx) scrut

      Term.Let var term body -> Tree.Let var <$> go rho [] term <*> go rho ctx body

      Term.Fun fun -> do
        tree <- transform (n - 1) ctx $ Term.Fun fun
        findRenaming tree rho >>= \case
          Just tr -> pure tr
          Nothing -> findGeneralisation tree rho >>= \case
            Just (Prog term env') -> withEnv env' do go rho [] term
            Nothing -> do
              fun' <- fresh "fun"
              Prog term env' <- residualise tree
              let args = Tree.freeVars tree
              let term' = unfold env' term
              body <- withEnv env' do go ((Tree.Use fun' args, tree):rho) [] term'
              pure $ mkDef fun' args body

    go' rho ctx tree = case ctx of

      [] -> pure tree

      Apply arg:ctx' -> do
        arg' <- go rho [] arg
        go' rho ctx' $ Tree.App tree arg'

      Match alts:ctx' -> do
        alts' <- for alts \(Term.Alt pars body) -> Tree.Alt pars <$> go rho ctx' body
        pure $ Tree.Case tree alts'


distill :: MonadFresh m => Int -> Term -> ReaderT Defs m Tree
distill n = go [] []

  where

    go rho ctx = \case

      Term.Var var -> go' rho ctx (Tree.Var var)

      Term.Con con args -> case ctx of
        []              -> Tree.Con con <$> traverse (go rho []) args
        Match alts:ctx' -> go rho ctx' =<< reduceCase con args alts
        Apply _   :_    -> error "Constructor applied"

      Term.Lam par body -> case ctx of
        []             -> Tree.Lam par <$> go rho [] body
        Apply arg:ctx' -> go rho ctx' =<< reduceLam par arg body
        Match _  :_    -> error "Lambda scrutineed"

      Term.App fun arg -> go rho (Apply arg:ctx) fun

      Term.Case scrut alts -> go rho (Match alts:ctx) scrut

      Term.Let var term body -> Tree.Let var <$> go rho [] term <*> go rho ctx body

      Term.Fun fun -> do
        tree <- transform n ctx $ Term.Fun fun
        findRenaming tree rho >>= \case
          Just tr -> pure tr
          Nothing -> findGeneralisation tree rho >>= \case
            Just (Prog term env') -> withEnv env' do distill (n + 1) term
            Nothing -> do
              fun' <- fresh "fun"
              let args = Tree.freeVars tree
              Prog term env' <- residualise tree
              let term' = unfold env' term
              body <- withEnv env' $ go ((Tree.Use fun' args, tree):rho) [] term'
              pure $ mkDef fun' args body

    go' rho ctx tree = case ctx of

      [] -> pure tree

      Apply arg:ctx' -> do
        arg' <- go rho [] arg
        go' rho ctx' $ Tree.App tree arg'

      Match alts:ctx' -> do
        alts' <- for alts \(Term.Alt pars body) -> Tree.Alt pars <$> go rho ctx' body
        pure $ Tree.Case tree alts'


findRenaming :: MonadFresh m => Tree -> [(Tree, Tree)] -> m (Maybe Tree)
findRenaming cur = firstJustM \(fun, old) -> traverse (`Tree.rename` fun) $ Tree.renaming old cur

findGeneralisation :: MonadFresh m => Tree -> [(Tree, Tree)] -> m (Maybe Prog)
findGeneralisation cur = firstJustM \(_, old) -> traverse residualise =<< Tree.generalisation old cur

unfold :: Defs -> Term -> Term
unfold env = go []
  where
    go ctx old = case old of
      Term.Var var -> place ctx $ Term.Var var
      Term.Con _ _ -> place ctx old
      Term.Lam _ _ -> place ctx old
      Term.Fun fun -> place ctx $ getDef fun env
      Term.Let var term body -> Term.Let var term $ go ctx body
      Term.App fun arg -> go (Apply arg:ctx) fun
      Term.Case scrut alts -> go (Match alts:ctx) scrut

usedBy :: Fun -> Tree -> Bool
usedBy fun = go
  where
    go = \case
      Tree.Var _ -> False
      Tree.Con _ args -> any go args
      Tree.App fun' arg -> go fun' || go arg
      Tree.Lam _ body -> go body
      Tree.Let _ term body -> go term || go body
      Tree.Case scrut alts -> go scrut || any (\(Tree.Alt _ e) -> go e) alts
      Tree.Use fun' _ -> fun == fun'
      Tree.Def _ _ body -> go body

mkDef :: Fun -> [Var] -> Tree -> Tree
mkDef fun args body = if fun `usedBy` body
  then Tree.Def fun args body
  else body

withEnv :: MonadReader r m => r -> m a -> m a
withEnv env = local (const env)


reduceCase :: MonadFresh m => Con -> [Term] -> Term.Alts -> m Term
reduceCase con args alts = subst (associate pars args) body
  where
    Term.Alt pars body = findWithDefault (error "Unhandled case") con alts

reduceLam :: MonadFresh m => Var -> Term -> Term -> m Term
reduceLam = subst1
