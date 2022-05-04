module Distiller.Term.Parse ( parseProg, parseFile ) where

import Distiller.Name
import Distiller.Term
import Distiller.Prelude hiding ( try )
import Data.Set ( member, delete, (\\) )
import Data.Map.Strict ( alterF, keysSet )
import Data.Char ( isAlphaNum, isLower, isUpper )
import Data.Text qualified as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

parseProg :: FilePath -> Text -> Either String Prog
parseProg name = first errorBundlePretty . parse (usingReaderT pos1 $ hspaces *> progP <* eof) name

parseFile :: MonadIO m => FilePath -> m (Either String Prog)
parseFile name = parseProg name <$> readFile name


toProg :: Defs -> Maybe Prog
toProg (Defs defs_) = case alterF (, Nothing) (FunName $ MkName "main" 0) defs of
  (Nothing, _) -> Nothing
  (Just tm, d) -> Just $ Prog tm $ Defs d

  where

    defs :: Map Fun Term
    defs = go funs <$> defs_

    funs :: Set Fun
    funs = keysSet defs_

    go :: Set Fun -> Term -> Term
    go env = \case
      Var (VarName name) -> if FunName name `member` env
        then Fun (FunName name)
        else Var (VarName name)
      Fun fun -> Fun fun
      Con con args -> Con con $ go env <$> args
      App fun arg -> App (go env fun) (go env arg)
      Lam var@(VarName name) body -> Lam var $ go (shadow name env) body
      Let var@(VarName name) term body -> Let var (go env term) $ go (shadow name env) body
      Case scrut alts -> Case (go env scrut) $ alts <&> \(Alt pars body) -> Alt pars $ go (shadows pars env) body

    shadow :: Name -> Set Fun -> Set Fun
    shadow = delete . coerce

    shadows :: [Bndr] -> Set Fun -> Set Fun
    shadows vs env = env \\ fromList do coerce vs

progP :: Parser Prog
progP = do
  defs <- layout funcP <* eof
  toProg (fromList defs) `whenNothing` fail "No main function found"

funcP :: Parser (Fun, Term)
funcP = bindingP funP

bindP :: Parser (Var, Term)
bindP = bindingP varP

bindingP :: Parser a -> Parser (a, Term)
bindingP lblP = label "function binding" do
  name <- lblP
  args <- many varP
  symbol "="
  term <- termP
  pure (name, Lams args term)

termP :: Parser Term
termP = label "expression" $ asum @[]
  [ do
      func <- atermP
      args <- many atermP
      pure $ Apps func args
  , do
      symbol "\\"
      pars <- some varP
      symbol "->"
      expr <- termP
      pure $ Lams pars expr
  , do
      symbol "let"
      defs <- layout bindP
      symbol "in"
      body <- termP
      pure $ foldr (uncurry Let) body defs
  , do
      symbol "case"
      scrut <- termP
      symbol "of"
      cases <- layout altP
      pure $ Case scrut $ fromList cases
  ]

atermP :: Parser Term
atermP = label "atomic expression" $ asum @[]
  [ Var <$> varP
  , Con <$> conP <*> fmap fold do optional do brackets $ termP `sepBy` symbol ","
  , parens termP
  ]

altP :: Parser (Con, Alt)
altP = label "alternative" $ (,) <$> conP <*> do Alt <$> (fold <$> optional do brackets $ varP `sepBy` symbol ",") <* symbol "->" <*> termP

type Parser = ReaderT Pos (Parsec Void Text)

hspaces :: Parser ()
hspaces = Lex.space spaces lcomment bcomment
  where
    lcomment = Lex.skipLineComment "--"
    bcomment = Lex.skipBlockCommentNested "{-" "-}"
    spaces = try do
      space1
      ref <- ask
      cur <- Lex.indentLevel
      end <- isJust <$> optional eof
      when (cur < ref && not end) $ Lex.incorrectIndent GT ref cur

layout :: Parser a -> Parser [a]
layout p =
  do
    cur <- Lex.indentLevel
    r <- local (const cur) $ many $ local (const $ inc cur) p <* hspaces
    hspaces
    pure r
  <|> braces (p `sepEndBy` symbol ";")
  <|> pure []
  where
    inc = mkPos . (+ 1) . unPos

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme hspaces

symbol :: Text -> Parser ()
symbol = void . Lex.symbol hspaces

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

idLikeP :: Parser Text
idLikeP = lexeme $ takeWhile1P (Just "identifier") \t -> isAlphaNum t || t == '_' || t == '\''

varLikeP :: Parser Text
varLikeP = do
  res <- idLikeP
  guard $ isLower $ Text.head res
  pure res

conLikeP :: Parser Text
conLikeP = do
  res <- idLikeP
  guard $ isUpper $ Text.head res
  pure res

reservedVarNames :: Set Text
reservedVarNames = ["let", "in", "case", "of"]

conP :: Parser Con
conP = label "constructor" $ try $ ConName . (`MkName` 0) . encodeUtf8 <$> conLikeP

varP :: Parser Var
varP = label "variable" $ try $ varLikeP >>= \x -> if x `member` reservedVarNames
  then fail $ show x <> " cannot be used as variable name"
  else pure $ VarName $ MkName (encodeUtf8 x) 0

funP :: Parser Fun
funP = label "function" $ try $ varLikeP >>= \x -> if x `member` reservedVarNames
  then fail $ show x <> " cannot be used as function name"
  else pure $ FunName $ MkName (encodeUtf8 x) 0
