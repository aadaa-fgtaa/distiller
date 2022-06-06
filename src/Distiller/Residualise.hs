module Distiller.Residualise where

import Distiller.Name
import Distiller.Tree as Tree
import Distiller.Term as Term
import Distiller.Term.Print
import Distiller.Prelude
import Data.List.Extra ( firstJust )

residualise :: MonadFresh m => Tree.Tree -> m Term.Prog
residualise = fmap (uncurry Term.Prog) . runWriterT . evaluatingStateT [] . go
  where
    go tree = case tree of

      Tree.Use fun args -> get >>= \rho -> case firstJust (\(_, defHeader, useHeader) -> (, useHeader) <$> Term.renaming defHeader header) rho of
        Just (sub, useHeader) -> Term.rename sub useHeader
        Nothing -> error $ "No matching Def node for " <> showPretty header
        where
          header = Term.Header fun args

      Tree.Def fun args body -> get >>= \rho -> case firstJust (\(_, defHeader, useHeader) -> (, useHeader) <$> Term.renaming defHeader header) rho of
        Just (sub, useHeader) -> Term.rename sub useHeader
        Nothing -> case firstJust (\(orig, _, useHeader) -> (, useHeader) <$> Tree.renaming orig tree) rho of
          Just (sub, useHeader) -> Term.rename sub useHeader
          Nothing -> do
            fun' <- clone fun
            let
              args' = Tree.freeVars body
              defHeader = Term.Header fun  args
              useHeader = Term.Header fun' args'
            modify' ((tree, defHeader, useHeader):)
            body' <- go body
            tell [(fun', Term.Lams args' body')]
            pure useHeader
        where
          header = Term.Header fun args

      Tree.Var var -> do
        pure $ Term.Var var

      Tree.Con con args -> do
        Term.Con con <$> traverse go args

      Tree.Lam par body -> do
        Term.Lam par <$> go body

      Tree.App fun arg -> do
        Term.App <$> go fun <*> go arg

      Tree.Let var term body -> do
        body' <- go body
        term' <- go term
        pure $ Term.Let var term' body'

      Tree.Case scrut alts -> do
        Term.Case <$> go scrut <*> for alts \(Tree.Alt pars body) -> Term.Alt pars <$> go body
