# Functional Adventure - the "we're moving out" version

A text based adventure game written in Haskell.

## New Features

### Health:

As a player you start out with 100 health units which will fluctuate over the course of the game in line with the following rules:

1. Each time you move you will lose 10 health units
2. When you pick up an object, your health will decrease by 10% of the object's weight. In other words, say your health is still at 100. If you pick up a stove that is 70 pounds, it'll cost 7 health units and your health will be 93.
3. You can gain health by eating. Each thing you eat will increase your health by however much it weights. However, you can only eat things that you've already picked up, i.e. things that are in your inventory. You can also only eat things that are edible—no stoves for dinner. You eat an object by typing "eat <object>". For example, if I had successfully acquired an apple, I could eat it by typing "eat apple".
4. You will die if your health drops to 0. The game will end with the messages: "You ran out of health and have died. Good luck next time!"
5. Finally, by typing "health" you are able to see how much health you have at any given time.

### Game Objective

As a player, your objective is to move every object into the yard. I was inpsired by the chaos of my last couple days in Hyde Park, trying to get everything I owned all packed up. You can do this by taking items and dropping items. However, given the health constraint, you must consider the order in which you enter each room, which items to pick up, and when to eat the food you have at your disposal. Once you drop all the objects into the yard, you will receive the message that you've won the game.

If you are curious about how close you are along the way or have forgotten how many objects have made it into the living room at any given time, you can type "status" and the game will return the number of objects you've moved into the living room out of the total number of objects.

## Code Review

### Slippery Objects

To implement the slippery Jug, I added a slipperyMoves parameter of type Maybe Int to the Item datatype. I set slipperyMoves to Nothing for all the items except for Jug, for which I set slipperyMoves to 3. I then added to the move function in GameState.hs. After verifying that a player had successfully moved to a new room, I checked to see if they were carrying a slippery item, the jug. If they were carrying the jug and the jug's slipperyMoves value had reached 0, the jug would fall out of the player's inventory and the message would say "You feel something slip out of your pocket." However, if the jug's slipperyMoves was a positive integer, because the player had moved into a new room, slipperyMoves would decrease by 1. Further, thanks to a helper function dropItemSlippery, voluntarily dropping the item does not reset the clock on it slipping. My implementation satisfies all of the other requirements outlined in the doc.

### Health

I added a health parameter to the Player datatype and made adjustments to my addItem function such that each item added to the inventory would come at some cost to the player's health. I included a food list in Item.hs in order to specify exactly which items could be eaten. I added an eatItem function that added to the player's health, and implemented corresponding functions in Command.hs, GameState.hs, and GameIO.hs in order to link the eatItem functionality to a command "eat" in the game itself. A player's health is capped at 100.

I wrote hasDied and finishGameDied in order to end the game if the player's health runs out.

### Game Objective

Implementing this was fairly straightforward. I simply changed haveWonGame in GameState.hs to reflect the goal outlined above: you win the game once all the objects have made it into the yard.

### Runtime Bugs

I didn't run into any runtime bugs after playing this game many many times. Hopefully there are none to discover! For reference, I've included a sequence of steps that lead to victory in a file called winSteps.txt in the test folder.
