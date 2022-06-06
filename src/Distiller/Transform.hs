module Distiller.Transform where

import Distiller.Name
import Distiller.Tree as Tree
import Distiller.Term as Term
import Distiller.Prelude
import Distiller.Residualise
import Data.Set ( member, insert )
import Data.Foldable.Extra ( firstJustM )
import qualified Data.IntMap.Strict as IntMap

-- TODO Proper error handling
-- TODO Caching implemented here is broken - it can cause out-of-scope fold nodes. Just for quick experiments

type Cache = IntMap ([(Term, Tree)], [(Tree, Tree)])
type CacheT = StateT Cache

transformProg :: MonadFresh m => Int -> Prog -> m Prog
transformProg n (Prog main defs) = residualise =<< usingReaderT defs do evaluatingStateT [] do transform n [] main

distillProg :: MonadFresh m => Prog -> m Prog
distillProg (Prog main defs) = residualise =<< usingReaderT defs do evaluatingStateT [] do distill 0 main


transform :: MonadFresh m => Int -> [Context] -> Term -> CacheT (ReaderT Defs m) Tree
transform lvl ctx term | lvl <= 0 = go [] (place ctx term)
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

      Term.Let var def body -> do
        Tree.Let var <$> go rho def <*> go rho body

      Term.Case scrut alts -> do
        Tree.Case <$> go rho scrut <*> for alts \(Term.Alt pars body) -> Tree.Alt pars <$> go rho body

transform lvl ctx0 term0 = go [] ctx0 term0

  where

    go rho ctx term = case term of

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

      Term.Let var def body -> Tree.Let var <$> go rho [] def <*> go rho ctx body

      Term.Fun _fun -> withTryReturnM \tryReturnM -> do
        tryReturnM $ findPreCached lvl ctx term

        tree <- lift $ transform (lvl - 1) ctx term

        tryReturnM $ findPostCached lvl tree
        tryReturnM $ findRenaming tree rho
        tryReturnM $ findGeneralisation tree rho $ go rho []

        fun' <- fresh "fun"
        Prog term' env' <- residualise tree
        let args = Tree.freeVars tree
        let term'' = unfold env' term'
        body <- lift $ withEnv env' do go ((Tree.Use fun' args, tree):rho) [] term''
        let res = mkDef fun' args body
        cache lvl ctx term tree res
        pure res

    go' rho ctx tree = case ctx of

      [] -> pure tree

      Apply arg:ctx' -> do
        arg' <- go rho [] arg
        go' rho ctx' $ Tree.App tree arg'

      Match alts:ctx' -> do
        alts' <- for alts \(Term.Alt pars body) -> Tree.Alt pars <$> go rho ctx' body
        pure $ Tree.Case tree alts'


distill :: MonadFresh m => Int -> Term -> CacheT (ReaderT Defs m) Tree
distill lvl = go [] []

  where

    go rho ctx term = case term of

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

      Term.Let var def body -> Tree.Let var <$> go rho [] def <*> go rho ctx body

      Term.Fun _fun -> withTryReturnM \tryReturnM -> do
        tryReturnM $ findPreCached (-lvl) ctx term

        tree <- lift $ transform lvl ctx term

        tryReturnM $ findPostCached (-lvl) tree
        tryReturnM $ findRenaming tree rho
        tryReturnM $ findGeneralisation tree rho $ distill (lvl + 1)

        fun' <- fresh "fun"
        let args = Tree.freeVars tree
        Prog term' env' <- residualise tree
        let term'' = unfold env' term'
        body <- lift $ withEnv env' $ go ((Tree.Use fun' args, tree):rho) [] term''
        let res = mkDef fun' args body
        cache (-lvl) ctx term tree res
        pure res


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

findGeneralisation :: (MonadFresh m, MonadReader Defs m) => Tree -> [(Tree, Tree)] -> (Term -> m b) -> m (Maybe b)
findGeneralisation cur rho go = firstJustM (\(_, old) -> traverse residualise =<< Tree.generalisation old cur) rho >>= traverse \(Prog term env') -> withEnv env' do go term

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


findPreCached :: (MonadState Cache m, MonadFresh m) => Int -> [Context] -> Term -> m (Maybe Tree)
findPreCached n ctx tm = gets (fst . IntMap.findWithDefault mempty n) >>= firstJustM \(term, tree) -> traverse (`Tree.rename` tree) $ Term.renaming term cur
  where
    cur = place ctx tm

findPostCached :: (MonadState Cache m, MonadFresh m) => Int -> Tree -> m (Maybe Tree)
findPostCached n tr = gets (snd . IntMap.findWithDefault mempty n) >>= firstJustM \(term, tree) -> traverse (`Tree.rename` tree) $ Tree.renaming term tr

cache :: MonadState Cache m => Int -> [Context] -> Term -> Tree -> Tree -> m ()
cache lvl ctx term tree res = modify' $ upsert lvl $ bimap ((place ctx term, res):) ((tree, res):)
  where
    upsert key f = IntMap.alter (Just . f . fold) key
