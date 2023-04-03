module Room where

import Item
import Direction 
import Data.List

data RoomName
  = Kitchen
  | Pantry
  | Yard
  | LivingRoom
  | Bedroom
  deriving (Eq, Ord)

instance Show RoomName where
    show Kitchen = "kitchen"
    show Pantry = "pantry"
    show Yard = "yard"
    show LivingRoom = "living room"
    show Bedroom = "bedroom"

type Exit = (Direction, RoomName)

data Room = Room {rname :: RoomName,
                  desc :: String,
                  exits :: [Exit],
                  objects :: [ItemName]}
                  deriving(Show, Eq)

kitchen :: Room
kitchen = Room Kitchen "You are in a small kitchen" 
          [(N, LivingRoom), (S, Yard), (E, Pantry)] [Pot, Stove, Grill, Salmon]

pantry :: Room
pantry = Room Pantry "You are in a pantry" [(W, Kitchen)] [Tarragon, Beans, Cow, Steak, Watermelon]

yard :: Room 
yard = Room Yard "You are in a yard" [(N, Kitchen)] []

livingRoom :: Room 
livingRoom = Room LivingRoom "You are in the living room" 
             [(N, Bedroom), (S, Kitchen)] [Couch, Jug, Sandbag, Orange]

bedroom :: Room 
bedroom = Room Bedroom "You are in the bedroom" [(S, LivingRoom)] [Bed, Apple]

-- use constants defiend for rooms to produce a list of names 
roomNames :: [RoomName]
roomNames = map rname allRooms

addItem :: ItemName -> Room -> Room
addItem itemName room = room {objects = itemName : objects room}

removeItem :: ItemName -> Room -> Room
removeItem itemName room = room {objects = delete itemName (objects room)}

allRooms :: [Room]
allRooms = [kitchen, pantry, yard, livingRoom, bedroom]

hasObjects :: Room -> Bool
hasObjects room = objects room /= []

