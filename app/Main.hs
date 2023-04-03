module Main where

import Control.Monad.State

import GameState
import GameIO

main :: IO ()
main = do
    putStrLn "Welcome to Functional Adventure! You're moving out! \nIn order to win the game, you must move every object in your home to the yard. \nYou can check your progress by typing \"status\". \nBeware of slippery objects, and make sure you eat enough food to keep your health up. \nGood luck!"  
    evalStateT (forever repl) initialState
