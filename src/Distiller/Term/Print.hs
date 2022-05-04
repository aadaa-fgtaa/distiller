{-# OPTIONS_GHC -Wno-orphans #-}

module Distiller.Term.Print where

import Distiller.Name
import Distiller.Term
import Distiller.Prelude hiding ( (<+>) )
import Prettyprinter
import Prettyprinter.Render.Text
import System.IO ( hPutStrLn )

showPretty :: Pretty a => a -> Text
showPretty = renderStrict . layoutPretty (LayoutOptions Unbounded) . pretty

printPretty :: MonadIO m => Pretty a => a -> m ()
printPretty = hPrintPretty stdout

hPrintPretty :: MonadIO m => Pretty a => Handle -> a -> m ()
hPrintPretty h a = liftIO $ renderIO h (layoutPretty (LayoutOptions Unbounded) $ pretty a) >> hPutStrLn h ""


instance Pretty Prog where
  pretty (Prog main defs) = "main =" <+> pretty main </> pretty defs

instance Pretty Defs where
  pretty = vsep . fmap (uncurry pBind) . bindings

instance Pretty Term where
  pretty = go False True
    where
      go atomic greedy = \case
        Var var -> pretty var
        Fun fun -> pretty fun
        Con con []   -> pretty con
        Con con args -> pretty con <> list do go False True <$> args
        App fun arg -> parensIf atomic $ go False False fun <+> go True (greedy || atomic) arg
        Lam par body -> parensIf (not greedy || atomic) $ "\\" <> pretty par <> " -> " <> go False True body
        Let var def body -> parensIf (not greedy || atomic) $ "let" <+> pBind var def <+> "in" <+> pretty body
        Case scrut alts -> parensIf (not greedy || atomic) $ "case" <+> pretty scrut <+> "of" <> nested do pAlts alts

pBind :: Pretty lbl => lbl -> Term -> Doc ann
pBind name (Lams []   body) = pretty name                            <+> "=" <+> pretty body
pBind name (Lams pars body) = pretty name <+> hsep (pretty <$> pars) <+> "=" <+> pretty body

pAlts :: Alts -> Doc ann
pAlts alts = vsep $ pAlt <$> assocs alts

pAlt :: (Con, Alt) -> Doc ann
pAlt (con, Alt []   body) = pretty con                           <+> "->" <+> pretty body
pAlt (con, Alt pars body) = pretty con <> list (pretty <$> pars) <+> "->" <+> pretty body


instance Pretty Var where
  pretty (VarName name) = pretty name

instance Pretty Fun where
  pretty (FunName name) = pretty name

instance Pretty Con where
  pretty (ConName name) = pretty name

instance Pretty Name where
  pretty (MkName label 0) = fromString (decodeUtf8 label)
  pretty (MkName label u) = fromString (decodeUtf8 label) <> "_" <> pretty u


nested :: Doc ann -> Doc ann
nested doc = nest 2 $ hardline <> doc

infixr 5 </>
(</>) :: Doc ann -> Doc ann -> Doc ann
l </> r = l <> hardline <> r

parensIf :: Bool -> Doc ann -> Doc ann
parensIf cond = if cond then parens else id
