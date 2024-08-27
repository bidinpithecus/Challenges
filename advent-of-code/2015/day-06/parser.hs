module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Commands
-- toggle 461,550 through 564,900
-- turn on 461,550 through 564,900
-- turn off 461,550 through 564,900

data Command
  = TurnOn (Int, Int) (Int, Int)
  | TurnOff (Int, Int) (Int, Int)
  | Toggle (Int, Int) (Int, Int)
  deriving (Show)

commandParser :: Parser Command
commandParser =
  try turnOnParser
    <|> try turnOffParser
    <|> try toggleParser

turnOnParser :: Parser Command
turnOnParser = do
  _ <- string "turn on "
  coordStart <- coordParser
  _ <- string " through "
  TurnOn coordStart <$> coordParser

turnOffParser :: Parser Command
turnOffParser = do
  _ <- string "turn off "
  coordStart <- coordParser
  _ <- string " through "
  TurnOff coordStart <$> coordParser

toggleParser :: Parser Command
toggleParser = do
  _ <- string "toggle "
  coordStart <- coordParser
  _ <- string " through "
  Toggle coordStart <$> coordParser

coordParser :: Parser (Int, Int)
coordParser = do
  x <- read <$> many1 digit
  _ <- char ','
  y <- read <$> many1 digit
  return (x, y)

parseCommand :: String -> Either ParseError Command
parseCommand = parse commandParser ""
