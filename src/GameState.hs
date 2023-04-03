module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction
import GHC.RTS.Flags (GCFlags(initialStkSize))

data KeyError = KeyError
  deriving Show

instance Exception KeyError

type GameMap = M.Map RoomName Room 

type Error a = Either String a

data GameState = GameState {message :: Maybe String, 
                  gmap :: GameMap,
                  universe :: Universe,
                  player :: Player}
                  deriving(Show)

-- Takes in a list of rooms and constructs a GameMap
mkMap :: [Room] -> GameMap
mkMap ls = M.fromList (help ls)
    where 
        help [] = [] 
        help (x:xs) = (rname x, x) : help xs

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState
initialState = GameState Nothing gameMap univ you

-- Don't ever call getObject or getRoom on a string literal 
-- The only thing you should call getObject or getRoom on is an ItemName or RoomName
-- you have extracted from a GameState that was made using the mkUniverse function.
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rname newRoom gmap 
    = case M.lookup rname gmap of 
        Just _ -> M.insert rname newRoom gmap
        Nothing -> gmap

setMessage :: String -> GameState -> GameState
setMessage str gs 
    = case str of 
        "" -> gs {message = Nothing}
        _ -> gs {message = Just str}

currentInventory :: GameState -> [ItemName]
currentInventory gs = inventory (player gs)

currentRoom :: GameState -> Room
currentRoom gs = getRoom (location (player gs)) gs

nearbyObjects :: GameState -> [ItemName]
nearbyObjects gs = objects (currentRoom gs)

takeItemOld :: ItemName -> GameState -> GameState
takeItemOld iname gs = let
    oldRoom = currentRoom gs
    newRoom = Room.removeItem iname oldRoom 
    newPlayer = Player.addItem iname (player gs)
    newGmap = setRoomMap (rname oldRoom) newRoom (gmap gs)
    newMessage = Just ("You take the " ++ show iname ++ ".")
    in gs {message = newMessage, gmap = newGmap, player = newPlayer}

takeItem :: ItemName -> GameState -> GameState
takeItem iname gs =
  let
    result = alreadyHaveTakeCheck iname gs
      >>= inRoomTakeCheck iname
      >>= weightCheck iname
  in
    case result of
      Left e -> setMessage e gs
      Right gs -> takeItemOld iname gs

dropItemOld :: ItemName -> GameState -> GameState
dropItemOld iname gs = let 
    oldRoom = currentRoom gs
    newRoom = Room.addItem iname oldRoom
    newPlayer = Player.removeItem iname (player gs)
    newGmap = setRoomMap (rname oldRoom) newRoom (gmap gs)
    newMessage = Just ("You drop the " ++ show iname ++ ".")
    in gs {message = newMessage, gmap = newGmap, player = newPlayer}

-- keeps the slippery item slipperyMoves 
dropItem :: ItemName -> GameState -> GameState 
dropItem iname gs = 
  let 
    result = anywhereDropCheck iname gs 
        >>= inRoomDropCheck iname 
    in 
      case result of 
        Left e -> setMessage e gs 
        Right gs -> dropItemOld iname gs

-- resets the Jug slipperyMoves to 3 because it was dropped automatically
dropItemSlippery :: ItemName -> GameState -> GameState 
dropItemSlippery iname gs = 
  let 
    result = anywhereDropCheck iname gs 
        >>= inRoomDropCheck iname 
    in 
      case result of 
        Left e -> setMessage e gs 
        Right gs -> 
            let gs' = dropItemOld iname gs
                in 
                    if isSlippery iname  
                        then gs' { universe = M.adjust (\i -> i {slipperyMoves = Just 3}) iname (universe gs')}
                    else gs'

eatItemHelper :: ItemName -> GameState -> GameState
eatItemHelper iname gs = 
    if findWeight iname + health (player gs) <= 100 then let gainedHealth = findWeight iname in
        let 
        newPlayer = Player.eatItem iname (player gs)
        newMessage = Just ("You eat the " ++ show iname ++ " and have gained " ++ show gainedHealth ++ " health. Your health is now " ++ show (health newPlayer) ++ ".") 
        newUniverse = deleteItem iname (universe gs)
        in gs {message = newMessage, player = newPlayer, universe = newUniverse}
    else let gainedHealth = 100 - health (player gs) in 
        let 
        newPlayer = Player.eatItem iname (player gs)
        newMessage = Just ("You eat the " ++ show iname ++ " and have gained " ++ show gainedHealth ++ " health. Your health is now " ++ show (health newPlayer) ++ ".") 
        newUniverse = deleteItem iname (universe gs)
        in gs {message = newMessage, player = newPlayer, universe = newUniverse}

eatItemGs :: ItemName -> GameState -> GameState 
eatItemGs iname gs = case
    isInInventory iname gs of 
        Left isInInventory -> gs {message = Just isInInventory}
        Right _ -> case isEdible iname gs of 
            Left isEdibleError -> gs {message = Just isEdibleError}
            Right _ -> eatItemHelper iname gs

-- returns error if the food is not edible 
isEdible :: ItemName -> GameState -> Error GameState 
isEdible iname gs = if iname `elem` food 
    then Right gs 
    else Left ("The " ++ show iname ++ " is not edible.")

-- returns error if the food is not in your inventory
isInInventory :: ItemName -> GameState -> Error GameState
isInInventory iname gs = if iname `elem` currentInventory gs
    then Right gs
    else Left ("The " ++ show iname ++ " cannot be eaten because it isn't in your inventory.")

-- returns total weight of inventory that Player is carrying
inventoryWeight :: GameState -> Integer 
inventoryWeight gs = sum (fmap (\x -> weight (getObject x gs)) (inventory (player gs)))

-- returns error if the player is already carrying item
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gs = if iname `elem` currentInventory gs
    then Left ("You are already carrying the " ++ show iname ++ ".")
    else Right gs

-- returns gs if player is in room with item 
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gs = if iname `elem` nearbyObjects gs
    then Right gs
    else Left ("There is no " ++ show iname ++ " in this room.")

-- returns error if new item exceeds total weight of player
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gs = if (inventoryWeight gs + weight (getObject iname gs)) > maxWeight (player gs)
    then Left "That's too much weight for you to carry."
    else Right gs

-- return error if item isn't in current inventory or the current room 
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gs = if iname `elem` currentInventory gs || iname `elem` nearbyObjects gs
    then Right gs 
    else Left ("What do you mean, drop the " ++ show iname ++ "?")

-- return error if item is in the room the player is in
inRoomDropCheck :: ItemName -> GameState -> Error GameState 
inRoomDropCheck iname gs = if iname `elem` nearbyObjects gs 
    then Left ("You aren't carrying the " ++ show iname ++ ".")
    else Right gs 

-- true if there are any objects in the room the player is in
roomHasObjects :: GameState -> Bool
roomHasObjects gs = hasObjects (currentRoom gs)

-- returns exit in input direction in the input room
destinationName :: Direction -> Room.Room -> Maybe RoomName 
destinationName direction rm = lookup direction (Room.exits rm)
 
-- moves player in input direction
move :: Direction -> GameState -> GameState 
move direction gs = 
    let newRoom = destinationName direction (currentRoom gs) in 
        case newRoom of  
            Just name -> 
                let newMessage = Just ("You go " ++ show direction ++ ".")
                    newPlayer = newLocation name (player gs) in
                    let gs' = gs {message = newMessage, player = newPlayer} in
                        if isCarryingSlipperyItem (player gs') then
                            let item = getObject Jug gs in
                                case slipperyMoves item of
                                    Just 0 -> setMessage ("You go " ++ show direction ++ ".\nYou feel something slip out of your pocket...") (dropItemSlippery Jug gs') 
                                    Just moves -> setMessage ("You go " ++ show direction ++ ".\nYou have " ++ show moves ++ " moves remaining before the " ++ show Jug ++ " slips out of your inventory.") 
                                            gs { universe = M.adjust (\i -> i { slipperyMoves = Just (moves - 1) }) Jug (universe gs'),
                                                 player = newPlayer }
                                    Nothing -> gs'
                        else gs'
            _ -> let newMessage = Just "There is no exit in that direction."
                    in gs {message = newMessage}

haveWonGame :: GameState -> Bool
haveWonGame gs = sort (objects (getRoom Yard gs)) == sort [Stove, Pot, Couch, Sandbag, Jug, Grill, Bed]

hasDied :: GameState -> Bool
hasDied gs = health (player gs) <= 0

status :: GameState -> Integer 
status gs = toInteger (length (objects (getRoom Yard gs)))