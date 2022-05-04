{-# LANGUAGE UndecidableInstances #-}

module Distiller.Term where

import Distiller.Name
import Distiller.Solver
import Distiller.Prelude
import Data.Map.Strict ( insert )

data Prog = Prog Term Defs

newtype Defs = Defs { getDefs :: Map Fun Term }
  deriving newtype (Monoid, Semigroup, IsList)

bindings :: Defs -> [(Fun, Term)]
bindings = assocs . getDefs

getDef :: Fun -> Defs -> Term
getDef fun@(FunName name) = findWithDefault (error $ "Out of scope function: " <> show name) fun . getDefs

askDef :: MonadReader Defs m => Fun -> m Term
askDef = asks . getDef


data Term
  = Var Var
  | Fun Fun
  | Con Con [Term]
  | App Term Term
  | Lam Bndr Term
  | Let Bndr Term Term
  | Case Term Alts

type Alts = Map Con Alt

data Alt = Alt [Bndr] Term


type Substitution = Map Var Term

subst :: MonadFresh m => Substitution -> Term -> m Term
subst env = \case

  Var var -> do
    pure $ sub var

  Fun fun -> do
    pure $ Fun fun

  Con con args -> do
    Con con <$> traverse (subst env) args

  App fun arg -> do
    App <$> subst env fun <*> subst env arg

  Lam par body -> do
    par' <- clone par
    Lam par' <$> subst (bind par par') body

  Let var term body -> do
    var' <- clone var
    Let var' <$> subst env term <*> subst (bind var var') body

  Case scrut alts -> Case <$> subst env scrut <*> for alts \(Alt pars body) -> do
    pars' <- traverse clone pars
    Alt pars' <$> subst (binds pars pars') body

  where

    sub var = findWithDefault (Var var) var env
    bind old new = insert old (Var new) env
    binds olds news = associate olds (Var <$> news) <> env

subst1 :: MonadFresh m => Var -> Term -> Term -> m Term
subst1 old new = subst [(old, new)]



type Renaming = Map Var Var

rename :: MonadFresh m => Renaming -> Term -> m Term
rename = subst . fmap Var

rename1 :: MonadFresh m => Var -> Var -> Term -> m Term
rename1 old = subst1 old . Var


renaming :: Term -> Term -> Maybe Renaming
renaming lhs' rhs' = execSolver $ unifyTerms lhs' rhs'

  where

    unifyTerms :: Term -> Term -> Solver Maybe ()
    unifyTerms
      (Var lvar)
      (Var rvar)
      = unifyVars lvar rvar

    unifyTerms
      (Fun lfun)
      (Fun rfun)
      = checkFuns lfun rfun

    unifyTerms
      (Con lcon largs)
      (Con rcon rargs)
      = checkCons lcon rcon >> sameZipWithM_ unifyTerms largs rargs

    unifyTerms
      (App lfun larg)
      (App rfun rarg)
      = unifyTerms lfun rfun >> unifyTerms larg rarg

    unifyTerms
      (Lam lpar lbody)
      (Lam rpar rbody)
      = bindVars lpar rpar do unifyTerms lbody rbody

    unifyTerms
      (Let lvar lterm lbody)
      (Let rvar rterm rbody)
      = unifyTerms lterm rterm >> bindVars lvar rvar do unifyTerms lbody rbody

    unifyTerms
      (Case lscrut lbody)
      (Case rsctut rbody)
      = unifyTerms lscrut rsctut >> unifyCases lbody rbody

    unifyTerms
      _
      _
      = empty

    unifyCases :: Alts -> Alts -> Solver Maybe ()
    unifyCases lalts ralts = do
      sameZipWithM_ unifyAlts (toAscList lalts) (toAscList ralts)

    unifyAlts :: (Con, Alt) -> (Con, Alt) -> Solver Maybe ()
    unifyAlts (lcon, Alt lpars lbody) (rcon, Alt rpars rbody) = do
      checkCons lcon rcon
      bindsVars lpars rpars (unifyTerms lbody rbody)


pattern Apps :: Term -> [Term] -> Term
pattern Apps fun args <- (collectApps -> (fun, args))
  where
    Apps = foldl' App
{-# COMPLETE Apps #-}

collectApps :: Term -> (Term, [Term])
collectApps = go []
  where
    go args (App fun arg) = go (arg:args) fun
    go args term          = (term, args)


pattern Lams :: [Bndr] -> Term -> Term
pattern Lams pars body <- (collectLams -> (pars, body))
  where
    Lams = flip $ foldr Lam
{-# COMPLETE Lams #-}

collectLams :: Term -> ([Bndr], Term)
collectLams = go
  where
    go (Lam par body) = let ~(pars, term) = go body in (par:pars, term)
    go term           = ([], term)


pattern Header :: Fun -> [Var] -> Term
pattern Header fun args <- (collectHeader -> Just (fun, args))
  where
    Header fun args = Apps (Fun fun) (Var <$> args)

collectHeader :: Term -> Maybe (Fun, [Var])
collectHeader = go []
  where
    go args (App fun (Var arg)) = go (arg:args) fun
    go args (Fun fun)           = Just (fun, args)
    go _    _                   = Nothing


data Context = Apply Term | Match Alts

place :: [Context] -> Term -> Term
place = flip $ foldl' \cur -> \case
  Apply arg -> App cur arg
  Match alts -> Case cur alts
