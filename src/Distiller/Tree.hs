module Distiller.Tree where

import Distiller.Name
import Distiller.Solver
import Distiller.Prelude
import Data.Set ( delete, intersection, (\\) )
import Data.Map.Strict ( insert )
import Pipes qualified as ListT
import Pipes.Prelude qualified as ListT

data Tree
  = Var Var
  | Con Con [Tree]
  | App Tree Tree
  | Lam Bndr Tree
  | Let Bndr Tree Tree
  | Case Tree Alts
  | Use Fun [Var]
  | Def Fun [Var] Tree

type Alts = Map Con Alt

data Alt = Alt [Bndr] Tree


type Substitution = Map Var Tree

type Renaming = Map Var Var

rename :: MonadFresh m => Renaming -> Tree -> m Tree
rename env = \case

  Var var -> do
    pure $ Var $ sub var

  Use fun args -> do
    pure $ Use fun $ sub <$> args

  Def fun pars body -> do
    Def fun (sub <$> pars) <$> rename env body

  Con con args -> do
    Con con <$> traverse (rename env) args

  App fun arg -> do
    App <$> rename env fun <*> rename env arg

  Lam par body -> do
    par' <- clone par
    Lam par' <$> rename (bind par par') body

  Let par term body -> do
    par' <- clone par
    Let par' <$> rename env term <*> rename (bind par par') body

  Case scrut alts -> Case <$> rename env scrut <*> for alts \(Alt pars body) -> do
    pars' <- traverse clone pars
    Alt pars' <$> rename (binds pars pars') body

  where

    sub var = findWithDefault var var env
    bind old new = insert old new env
    binds olds news = associate olds news <> env


rename1 :: MonadFresh m => Var -> Var -> Tree -> m Tree
rename1 old new = rename [(old, new)]


isRenaming :: Tree -> Tree -> Bool
isRenaming lhs rhs = isJust $ renaming lhs rhs

renaming :: Tree -> Tree -> Maybe Renaming
renaming lhs rhs = execSolver $ unifyTrees lhs rhs

  where

    unifyTrees :: Tree -> Tree -> Solver Maybe ()
    unifyTrees
      (Var lvar)
      (Var rvar)
      = unifyVars lvar rvar

    unifyTrees
      (Con lcon largs)
      (Con rcon rargs)
      = checkCons lcon rcon >> sameZipWithM_ unifyTrees largs rargs

    unifyTrees
      (App lfun larg)
      (App rfun rarg)
      = unifyTrees lfun rfun >> unifyTrees larg rarg

    unifyTrees
      (Lam lpar lbody)
      (Lam rpar rbody)
      = bindVars lpar rpar do unifyTrees lbody rbody

    unifyTrees
      (Let lvar lterm lbody)
      (Let rvar rterm rbody)
      = unifyTrees lterm rterm >> bindVars lvar rvar do unifyTrees lbody rbody

    unifyTrees
      (Case lscrut lbody)
      (Case rscrut rbody)
      = unifyTrees lscrut rscrut >> unifyCases lbody rbody

    unifyTrees
      (Def lfun _ lbody)
      (Def rfun _ rbody)
      = bindFuns lfun rfun do unifyTrees lbody rbody

    unifyTrees
      (Use lfun largs)
      (Use rfun rargs)
      = checkFuns lfun rfun >> sameZipWithM_ unifyVars largs rargs

    unifyTrees
      _
      _
      = empty

    unifyCases :: Alts -> Alts -> Solver Maybe ()
    unifyCases lalts ralts = do
      sameZipWithM_ unifyAlts (toAscList lalts) (toAscList ralts)

    unifyAlts :: (Con, Alt) -> (Con, Alt) -> Solver Maybe ()
    unifyAlts (lcon, Alt lpars lbody) (rcon, Alt rpars rbody) = do
      checkCons lcon rcon
      bindsVars lpars rpars (unifyTrees lbody rbody)


isEmbedding :: Tree -> Tree -> Bool
isEmbedding lhs rhs = isJust $ embedding lhs rhs

embedding :: Tree -> Tree -> Maybe Renaming
embedding lhs' rhs' = listToMaybe $ execSolver $ coupleTrees lhs' rhs'

  where

    unifyTrees :: Tree -> Tree -> Solver [] ()
    unifyTrees lhs rhs = coupleTrees lhs rhs <|> diveTrees lhs rhs

    coupleTrees :: Tree -> Tree -> Solver [] ()
    coupleTrees
      (Var lvar)
      (Var rvar)
      = unifyVars lvar rvar

    coupleTrees
      (Con lcon largs)
      (Con rcon rargs)
      = checkCons lcon rcon >> sameZipWithM_ unifyTrees largs rargs

    coupleTrees
      (App lfun larg)
      (App rfun rarg)
      = coupleTrees lfun rfun >> unifyTrees larg rarg

    coupleTrees
      (Lam lpar lbody)
      (Lam rpar rbody)
      = bindVars lpar rpar do coupleTrees lbody rbody

    coupleTrees
      (Let lvar lterm lbody)
      (Let rvar rterm rbody)
      = unifyTrees lterm rterm >> bindVars lvar rvar do coupleTrees lbody rbody

    coupleTrees
      (Case lscrut lalts)
      (Case rscrut ralts)
      = unifyTrees lscrut rscrut >> unifyCases lalts ralts

    coupleTrees
      (Def lfun _ lbody)
      (Def rfun _ rbody)
      = bindFuns lfun rfun do unifyTrees lbody rbody

    coupleTrees
      (Use lfun largs)
      (Use rfun rargs)
      = checkFuns lfun rfun >> sameZipWithM_ unifyVars largs rargs

    coupleTrees
      _
      _
      = empty

    unifyCases :: Alts -> Alts -> Solver [] ()
    unifyCases lalts ralts = do
      sameZipWithM_ unifyAlts (toAscList lalts) (toAscList ralts)

    unifyAlts :: (Con, Alt) -> (Con, Alt) -> Solver [] ()
    unifyAlts (lcon, Alt lpars lbody) (rcon, Alt rpars rbody) = do
      checkCons lcon rcon
      bindsVars lpars rpars (unifyTrees lbody rbody)

    diveTrees :: Tree -> Tree -> Solver [] ()
    diveTrees lhs = \case
      Var _ -> empty
      Con _ body -> altMap (unifyTrees lhs) body
      App fun arg -> unifyTrees lhs fun <|> unifyTrees lhs arg
      Lam _ body -> unifyTrees lhs body
      Let _ term body -> unifyTrees lhs term <|> unifyTrees lhs body
      Case scrut alts -> unifyTrees lhs scrut <|> faltMap alts \(Alt _ body) -> unifyTrees lhs body
      Use _ _ -> empty
      Def _ _ body -> unifyTrees lhs body


generalisation :: forall m . MonadFresh m => Tree -> Tree -> m (Maybe Tree)
generalisation lhs' rhs' = ListT.head $ ListT.enumerate do
  guard $ isEmbedding lhs' rhs'
  (genTree, genLets) <- runWriterT $ evalSolver $ matchTrees lhs' rhs'
  pure $ ifoldr Let genTree genLets
  where

    generaliseTrees :: Tree -> Tree -> Solver (WriterT Substitution (ListT m)) Tree
    generaliseTrees lhs rhs = matchTrees lhs rhs <|> abstractTrees lhs rhs

    matchTrees :: Tree -> Tree -> Solver (WriterT Substitution (ListT m)) Tree
    matchTrees
      (Var lvar)
      (Var rvar)
      = unifyVars lvar rvar $> Var rvar

    matchTrees
      (Con lcon largs)
      (Con rcon rargs)
      = checkCons lcon rcon >> Con rcon <$> sameZipWithM generaliseTrees largs rargs

    matchTrees
      (App lfun larg)
      (App rfun rarg)
      = App <$> generaliseTrees lfun rfun <*> generaliseTrees larg rarg

    matchTrees
      (Lam lpar lbody)
      (Lam rpar rbody)
      = Lam rpar <$> bindVars lpar rpar do generaliseTrees lbody rbody

    matchTrees
      (Let lvar lterm lbody)
      (Let rvar rterm rbody)
      = Let rvar <$> generaliseTrees lterm rterm <*> bindVars lvar rvar do generaliseTrees lbody rbody

    matchTrees
      (Case lscrut lalts)
      (Case rscrut ralts)
      = Case <$> generaliseTrees lscrut rscrut <*> generaliseCases lalts ralts

    matchTrees
      (Def lfun _     lbody)
      (Def rfun rargs rbody)
      = Def rfun rargs <$> bindFuns lfun rfun do generaliseTrees lbody rbody

    matchTrees
      (Use lfun largs)
      (Use rfun rargs)
      = checkFuns lfun rfun >> sameZipWithM_ unifyVars largs rargs $> Use rfun rargs

    matchTrees
      _
      _
      = empty

    generaliseCases :: Alts -> Alts -> Solver (WriterT Substitution (ListT m)) Alts
    generaliseCases lalts ralts = do
      fromList <$> sameZipWithM generaliseAlts (toAscList lalts) (toAscList ralts)

    generaliseAlts :: (Con, Alt) -> (Con, Alt) -> Solver (WriterT Substitution (ListT m)) (Con, Alt)
    generaliseAlts (lcon, Alt lpars lbody) (rcon, Alt rpars rbody) = do
      checkCons lcon rcon
      (rcon, ) . Alt rpars <$> bindsVars lpars rpars (generaliseTrees lbody rbody)

    abstractTrees :: Tree -> Tree -> Solver (WriterT Substitution (ListT m)) Tree
    abstractTrees _ tree = do
      gen <- fresh "gen"
      bounds <- toList . intersection (freeVarsSet tree) <$> rightBoundVars
      tell [(gen, Lams bounds tree)]
      pure $ Apps (Var gen) (Var <$> bounds)


freeVars :: Tree -> [Var]
freeVars = toList . freeVarsSet

freeVarsSet :: Tree -> Set Var
freeVarsSet = \case
  Var var -> [var]
  Use _ args -> fromList args
  Def _ _ body -> freeVarsSet body
  Con _ args -> foldMap freeVarsSet args
  App fun arg -> freeVarsSet fun <> freeVarsSet arg
  Lam par body -> delete par $ freeVarsSet body
  Let var term body -> freeVarsSet term <> delete var (freeVarsSet body)
  Case scrut alts -> freeVarsSet scrut <> ffoldMap alts \(Alt vars body) -> freeVarsSet body \\ fromList vars


pattern Apps :: Tree -> [Tree] -> Tree
pattern Apps fun args <- (collectApps -> (fun, args))
  where
    Apps = foldl' App
{-# COMPLETE Apps #-}

collectApps :: Tree -> (Tree, [Tree])
collectApps = go []
  where
    go args (App fun arg) = go (arg:args) fun
    go args term          = (term, args)


pattern Lams :: [Bndr] -> Tree -> Tree
pattern Lams pars body <- (collectLams -> (pars, body))
  where
    Lams = flip $ foldr Lam
{-# COMPLETE Lams #-}

collectLams :: Tree -> ([Bndr], Tree)
collectLams = go
  where
    go (Lam par body) = let ~(pars, term) = go body in (par:pars, term)
    go term           = ([], term)
