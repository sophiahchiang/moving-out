module Item where

import qualified Data.Map as M

data ItemName
  = Pot
  | Jug
  | Sandbag
  | Stove
  | Couch
  | Tarragon
  | Beans
  | Grill
  | Bed
  | Apple 
  | Orange 
  | Watermelon
  | Steak
  | Cow
  | Salmon 
  deriving (Eq, Ord)

instance Show ItemName where
  show Pot = "pot"
  show Jug = "jug"
  show Sandbag = "sandbag"
  show Stove = "stove"
  show Couch = "couch"
  show Tarragon = "tarragon"
  show Beans = "beans"
  show Grill = "grill"
  show Bed = "bed"
  show Apple = "apple"
  show Orange = "orange"
  show Watermelon = "watermelon"
  show Steak = "steak"
  show Cow = "cow"
  show Salmon = "salmon"

type Universe = M.Map ItemName Item

data Item = Item {iname :: ItemName,
                  weight :: Integer,
                  slipperyMoves :: Maybe Int}
                  deriving(Show, Eq)

-- Item and universe initializations 
pot :: Item
pot = Item Pot 4 Nothing

jug :: Item
jug = Item Jug 3 (Just 3)

sandbag :: Item
sandbag = Item Sandbag 25 Nothing

stove :: Item
stove = Item Stove 100 Nothing

couch :: Item
couch = Item Couch 280 Nothing

tarragon :: Item
tarragon = Item Tarragon 1 Nothing

beans :: Item
beans = Item Beans 30 Nothing

grill :: Item
grill = Item Grill 150 Nothing

bed :: Item
bed = Item Bed 300 Nothing

apple :: Item 
apple = Item Apple 2 Nothing 

orange :: Item 
orange = Item Orange 3 Nothing 

watermelon :: Item 
watermelon = Item Watermelon 11 Nothing

steak :: Item 
steak = Item Steak 5 Nothing 

cow :: Item 
cow = Item Cow 400 Nothing 

salmon :: Item 
salmon = Item Salmon 15 Nothing 

univ :: Universe
univ = mkUniverse [stove, pot, couch, sandbag, jug, grill, 
                  bed, tarragon, beans, apple, orange, watermelon,
                  steak, cow, salmon]

-- Takes in a list of items and constructs a Universe
-- For slippery items, you can set an initial number of moves before the item slips out
mkUniverse :: [Item] -> Universe
mkUniverse ls = M.fromList (help ls)
  where
    help [] = []
    help (x:xs) = (iname x, x) : help xs
      -- (iname x, x { slipperyMoves = if iname x `elem` slipperyItemNames then Just 3 else Nothing }) : help xs

itemNames :: [ItemName]
itemNames = M.keys univ

slipperyItemNames :: [ItemName]
slipperyItemNames = [Jug]

food :: [ItemName]
food = [Beans, Tarragon, Apple, Orange, Watermelon,
                  Steak, Cow, Salmon]

-- returns the weight of the item, given the itemName
findWeight :: ItemName -> Integer
findWeight itemName = maybe 0 weight (M.lookup itemName univ)

-- once an item is eaten it must be removed from the universe entirely
deleteItem :: ItemName -> Universe -> Universe
deleteItem itemName universe = M.delete itemName universe

isSlippery :: ItemName -> Bool
isSlippery itemName = itemName `elem` slipperyItemNames


