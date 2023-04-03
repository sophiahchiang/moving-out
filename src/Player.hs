module Player where 

import Item 
import Room

data Player = Player {inventory :: [ItemName],
                      maxWeight :: Integer,
                      location :: RoomName, 
                      health :: Integer}
                      deriving (Show, Eq)

-- Player initialization
you :: Player 
you = Player [] 400 Kitchen 100


-- takes in an item and a player
-- add an item to player's inventory
-- costs the player 10% of the item's weight in health to pick up an item 
addItem :: ItemName -> Player -> Player
addItem item p = let 
    weight = findWeight item
    fractionalWeight = fromIntegral weight * 0.1
    roundedWeight = round fractionalWeight in
    Player (item:inventory p) (maxWeight p) (location p) (health p - roundedWeight)

-- takes in an item and a player
-- remove an item from a player's inventory 
removeItem :: ItemName -> Player -> Player
removeItem item p = Player (filter (/= item) (inventory p)) (maxWeight p) (location p) (health p)

-- removes item from inventory and adds to their health
-- caps health at 100
eatItem :: ItemName -> Player -> Player 
eatItem iname p = if findWeight iname + health p <= 100 then
        Player (filter (/= iname) (inventory p)) (maxWeight p) (location p) (health p + findWeight iname)
    else 
        Player (filter (/= iname) (inventory p)) (maxWeight p) (location p) 100

-- takes in a RoomName and a player
-- change the location of the player
-- costs a player 10 health to change rooms
newLocation :: RoomName -> Player -> Player
newLocation roomName p = Player (inventory p) (maxWeight p) roomName (health p - 10)

-- takes in a player
-- true if player's inventory has something in it, false otherwise
isCarryingAnything :: Player -> Bool 
isCarryingAnything p = inventory p /= []

-- takes in a player
-- true if player's inventory has slipperyItem
isCarryingSlipperyItem :: Player -> Bool 
isCarryingSlipperyItem p = any isSlippery (inventory p)


