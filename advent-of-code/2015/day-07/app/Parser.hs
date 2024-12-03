module Parser where

import Data.Word (Word16)
import Text.Parsec
import Text.Parsec.String (Parser)

data Operand = Id String | Number Word16 deriving (Show)

data Op
  = AND Operand Operand String
  | OR Operand Operand String
  | LSHIFT Operand Operand String
  | RSHIFT Operand Operand String
  | ATTR Operand String
  | NOT Operand String
  deriving (Show)

opParser :: Parser Op
opParser =
  try andParser
    <|> try orParser
    <|> try lShiftParser
    <|> try rShiftParser
    <|> try attrParser
    <|> try notParser

andParser :: Parser Op
andParser = do
  operand1 <- operandParser
  _ <- string " AND "
  operand2 <- operandParser
  _ <- string " -> "
  AND operand1 operand2 <$> idParser

orParser :: Parser Op
orParser = do
  operand1 <- operandParser
  _ <- string " OR "
  operand2 <- operandParser
  _ <- string " -> "
  OR operand1 operand2 <$> idParser

lShiftParser :: Parser Op
lShiftParser = do
  operand1 <- operandParser
  _ <- string " LSHIFT "
  operand2 <- operandParser
  _ <- string " -> "
  LSHIFT operand1 operand2 <$> idParser

rShiftParser :: Parser Op
rShiftParser = do
  operand1 <- operandParser
  _ <- string " RSHIFT "
  operand2 <- operandParser
  _ <- string " -> "
  RSHIFT operand1 operand2 <$> idParser

attrParser :: Parser Op
attrParser = do
  operand <- operandParser
  _ <- string " -> "
  ATTR operand <$> idParser

notParser :: Parser Op
notParser = do
  _ <- string "NOT "
  operand <- operandParser
  _ <- string " -> "
  NOT operand <$> idParser

operandParser :: Parser Operand
operandParser =
  do
    Id <$> idParser
    <|> Number <$> intParser

idParser :: Parser String
idParser = do
  many1 lower

intParser :: Parser Word16
intParser = do
  read <$> many1 digit

parseOp :: String -> Either ParseError Op
parseOp = parse opParser ""
