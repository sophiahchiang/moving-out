module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item

type GameIO a = StateT GameState IO a

prompt :: GameIO ()
prompt = lift $ do
  putStr "-> "
  hFlush stdout

-- prints the message to the screen, 
-- then sets the game state message field to Nothing
printMessage :: GameIO ()
printMessage = do
  gs <- get
  case message gs of
    Just msg -> do
      lift $ putStrLn msg
      put gs { message = Nothing }
    Nothing -> return ()

-- prints a description of the room where the player is in the current game state
printDescription :: GameIO ()
printDescription = do
  gs <- get
  lift $ putStrLn (desc (currentRoom gs))

-- prints "You see the following objects:" 
-- followed by a list of all the items in the room where the player is
printObjects :: GameIO ()
printObjects = do
    gs <- get
    case nearbyObjects gs of
        [] -> return ()
        objects -> do
            lift $ putStrLn "You see the following objects:"
            lift $ mapM_ putStrLn (fmap show objects)

-- prints "There are exits in the following directions:" 
-- followed by a list of all the directions there are exits in, 
-- in the room where the player currently is
printExits :: GameIO ()
printExits = do
    gs <- get
    case exits (currentRoom gs) of
        [] -> return ()
        exit -> do
            lift $ putStrLn "There are exits in the following directions:"
            lift $ mapM_ putStrLn (fmap (\(direction,name) -> show direction) exit)

-- checks the player's current inventory
printInventory :: GameIO ()
printInventory = do
    gs <- get
    case currentInventory gs of
        [] -> lift $ putStrLn "You aren't carrying anything."
        items -> do
            lift $ putStrLn "You are carrying the following objects:"
            lift $ mapM_ putStrLn (fmap show items)

printHealth :: GameIO ()
printHealth = do 
  gs <- get
  lift $ putStrLn ("Your health is currently: " ++ show (health (player gs)))

printStatus :: GameIO ()
printStatus = do 
  gs <- get 
  lift $ putStrLn ("You have moved " ++ show (status gs) ++ " objects into the yard. "
                  ++ show (7 - status gs) ++ " to go!")

-- This should change the actual gameState as well... so player's inventory should change
-- Can I do this with map somehow? Even though I also need to printMessage everytime?
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()

actionOverList _ [] = pure ()
actionOverList action (item : items) = do
    gs <- get
    let newState = action item gs
    put newState
    printMessage
    actionOverList action items

finishGameWon :: GameIO ()
finishGameWon = do
    liftIO $ putStrLn "You successfully brought all of the items into the yard."
    liftIO $ putStrLn "Congrats! You win!"
    liftIO exitSuccess

finishGameDied :: GameIO ()
finishGameDied = do
  liftIO $ putStrLn "You ran out of health and have died"
  liftIO $ putStrLn "Good luck next time!"
  liftIO exitSuccess

exit :: GameIO ()
exit = do
    liftIO $ putStrLn "Goodbye!"
    liftIO exitSuccess

checkGameOver :: GameIO ()
checkGameOver = do
    gs <- get
    Control.Monad.State.when (hasDied gs) finishGameDied
    Control.Monad.State.when (haveWonGame gs) finishGameWon

syntaxError :: GameIO ()
syntaxError = liftIO $ putStrLn "I don't understand that."

opening :: GameIO ()
opening = do
  let message = "Welcome to Functional Adventure! You're moving out! "
        ++ "In order to win the game, you must move every object in your home to the yard. "
        ++ "Beware of slippery objects, and make sure you eat enough food to keep your health up. "
        ++ "Good luck!"
  liftIO $ putStrLn message

performCommand :: Command -> GameIO ()
performCommand Look = do
    printDescription
    printObjects
    printExits
performCommand (Move direction) = do
    gs <- get
    let newState = move direction gs
    put newState
    printMessage
performCommand Inventory = printInventory
performCommand (Take items) = actionOverList takeItem items
performCommand (Drop items) = actionOverList dropItem items
performCommand Exit = exit
performCommand (Eat items) = actionOverList eatItemGs items
performCommand Status = printStatus
performCommand Health = printHealth
performCommand _ = syntaxError

performConjunction :: Conjunction -> GameIO ()
performConjunction = mapM_ performCommand

parseConjunction :: String -> GameIO ()
parseConjunction input = do
  maybe syntaxError performConjunction (parseInput input)
  
repl :: GameIO ()
repl = do
  liftIO $ putStr "-> "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  parseConjunction input
  checkGameOver