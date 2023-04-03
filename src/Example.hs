module Example where

import Data.List
import System.Random
import qualified Data.Map as M

import Item
import Direction
import Room
import Player
import GameState

choose :: [a] -> IO a 
choose xs = do 
    i <- randomRIO (0,length xs - 1)
    return (xs !! i)

-- generate list that's as long as IO Int
exampleList :: IO a -> IO Int -> IO [a]
exampleList a b = do
    len <- b
    sequence (replicate len a)

class Example a where
    example :: IO a

instance Example Item where 
    example = do 
        itemName <- choose itemNames 
        weight <- randomRIO (0,100)
        return Item { iname = itemName, weight = weight, slipperyMoves = Nothing}

instance Example Direction where 
    example = choose [N, S, E, W]

exitExample :: IO Exit
exitExample = do 
        direction <- example
        room <- choose roomNames
        return (direction, room)

-- Objects chosen from objects defined in Item.hs
instance Example Room where 
    example = do 
        name <- choose roomNames
        let desc = "You are in the " ++ show name ++ ". It is a randomly-generated room."
        exits <- exampleList exitExample (randomRIO (2,4))
        objects <- exampleList (choose itemNames) (randomRIO (2,5)) 
        return Room { rname = name, desc = desc, exits = exits, objects = objects}

instance Example Player where 
    example = do 
        inventory <- exampleList example (randomRIO (0,10))
        health <- randomRIO (1,100)
        let weights = map weight inventory 
            max = maximum weights 
            min = minimum weights 
            diff = max - min 
            maxWeight = max + diff 
        location <- choose roomNames
        return Player { inventory = fmap iname (nub inventory), maxWeight = maxWeight, location = location, health = health}

instance Example GameState where 
    example = do 
        message <- choose [Just "One possible message.", Just "Yet another possible message", Nothing]
        player <- (example :: IO Player)
        rooms <- exampleList (example :: IO Room) (randomRIO (2,3))
        items <-  exampleList (example :: IO Item) (randomRIO (5,10))
        return GameState {message = message, gmap = mkMap rooms, universe = mkUniverse items, player = player}

