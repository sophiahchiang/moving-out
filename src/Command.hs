{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Command where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Eat [ItemName]
  | Health
  | Status 
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]
  
-- will parse any string representing an ItemName into
-- an inhabitant of the ItemName datatype
itemNameP :: Parser ItemName
itemNameP = choice
   [string "pot" *> pure Pot, 
    string "jug" *> pure Jug,
    string "sandbag" *> pure Sandbag,
    string "stove" *> pure Stove,
    string "couch" *> pure Couch,
    string "tarragon" *> pure Tarragon,
    string "beans" *> pure Beans,
    string "grill" *> pure Grill,
    string "bed" *> pure Bed,
    string "apple" *> pure Apple,
    string "orange" *> pure Orange, 
    string "watermelon" *> pure Watermelon,
    string "steak" *> pure Steak,
    string "cow" *> pure Cow, 
    string "salmon" *> pure Salmon]
 
-- takes alphabetic string off front of input
-- puts it in a singleton list
-- nounPhrase_stub = singleton <$> itemNameP
--   where singleton x = [x]
nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = sepBy1 itemNameP (string "," <* optional space)

-- only accepts string inventory
inventoryP :: Parser Command
inventoryP = Inventory <$ string "inventory"

-- parses the word 'take' plus a noun pharse into a Command FIX
takeP :: Parser Command 
takeP = Take <$> (string "take " *> nounPhrase_stub)

exitP :: Parser Command 
exitP = choice [string "exit" *> pure Exit, string "quit" *> pure Exit]

dropP :: Parser Command 
dropP = Drop <$> (string "drop " *> nounPhrase_stub)

lookP :: Parser Command 
lookP = Look <$ string "look"

directionP :: Parser Direction 
directionP = choice 
  [string "north" *> pure N, 
  string "south" *> pure S,
  string "east" *> pure E,
  string "west" *> pure W]

moveP :: Parser Command
moveP = Move <$> directionP

eatP :: Parser Command 
eatP = Eat <$> (string "eat " *> nounPhrase_stub)

healthP :: Parser Command
healthP = Health <$ string "health"

statusP :: Parser Command 
statusP = Status <$ string "status"

commandP :: Parser Command 
commandP = inventoryP <|> takeP <|> exitP <|> dropP <|> lookP <|> moveP <|> eatP <|> healthP <|> statusP

conjunctionP :: Parser Conjunction
conjunctionP = sepBy1 commandP (string " and ") <* eof

parseInput :: String -> Maybe Conjunction
parseInput s = case parsed of 
  Right x -> Just x
  _ -> Nothing 
  where parsed = parse conjunctionP s

