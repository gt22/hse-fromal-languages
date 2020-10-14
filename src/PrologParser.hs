module PrologParser where

import Control.Monad
import PrologAst
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef :: LanguageDef a
languageDef =
  emptyDef
    { Token.identStart = lower,
      Token.identLetter = alphaNum <|> char '_',
      Token.reservedNames = ["module", "type"],
      Token.reservedOpNames = [",", ";", "->", ":-"]
    }

lexer :: Token.TokenParser a
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

var :: Parser [Char]
var = do
  h <- upper
  t <- many (alphaNum <|> char '_')
  _ <- spaces
  return $ h : t

whiteSpace = Token.whiteSpace lexer

reservedOp = Token.reservedOp lexer

reserved = Token.reserved lexer

brackets = Token.parens lexer

dot = Token.dot lexer

(=>>) :: Monad m => m b -> m a -> m b
a =>> b = do
  x <- a
  _ <- b
  return x

spaced :: Parser a -> Parser a
spaced a = spaces >> a =>> spaces

parented :: Parser a -> Parser a
parented p = char '(' >> spaced p =>> char ')'

mulpar :: Parser a -> Parser a
mulpar a = parented a <|> a

abody :: Parser [Either Atom String]
abody =
  ( do
      h <- (Left <$> (parented (mulpar atom) <|> (pureAtom <$> identifier))) <|> (Right <$> var)
      t <- spaced abody
      return $ h : t
  )
    <|> ([] <$ spaces)

atom :: Parser Atom
atom = do
  h <- identifier
  t <- spaced abody
  return $ Atom h t

parseTerm :: Parser RelationBody
parseTerm = RAtom <$> atom <|> parented parseRelBody

parseConj :: Parser RelationBody
parseConj = foldr1 Conj <$> parseTerm `sepBy1` reservedOp ","

parseDisj :: Parser RelationBody
parseDisj = foldr1 Disj <$> parseConj `sepBy1` reservedOp ";"

parseRelBody :: Parser RelationBody
parseRelBody = parseDisj

relation :: Parser Relation
relation = do
  h <- atom
  b <- spaced $ optionMaybe $ reservedOp ":-" >> spaced parseRelBody 
  _ <- dot
  return $ Relation h b

parseModule :: Parser String
parseModule = reserved "module" >> identifier =>> dot

typeBody :: Parser Type
typeBody = parented typeExpr <|> TAtom <$> atom <|> Var <$> var

typeExpr :: Parser Type
typeExpr = foldr1 Arrow <$> typeBody `sepBy` spaced (reservedOp "->")

typ :: Parser TypeDef
typ = do
  _ <- reserved "type"
  h <- spaced identifier
  b <- spaced typeExpr =>> dot
  return $ TypeDef h b

prog :: Parser PrologProgram
prog = do
  m <- optionMaybe parseModule
  t <- many typ
  r <- many relation
  return $ Program m t r

parseString :: String -> Either ParseError PrologProgram
parseString =
  parse (prog =>> eof) ""
