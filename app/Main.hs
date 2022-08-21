module Main where

import CharacterCreation 

main :: IO ()
main = putStrLn "Hello, Haskell!"

data GameState = 
  GameState { getPlayer :: Player, getMonster :: Monster }

data Monster = 
  Monster { getMonsterName :: String, getMonsterHP :: Int }

gameState :: Player -> GameState
gameState = flip GameState (Monster "Orc" 10) 

start :: IO ()
start = do 
  game <- createCharacter gameState 
  beginEncounter game


-- Character creation
createCharacter :: (Player -> GameState) -> IO GameState
createCharacter gameState = do 
  putStrLn "Welcome to the text adventure!"
  putStrLn "What is your name, adventurer?" 
  name <- getName 
  case name of 
    Just name' -> do
      putStrLn "What a fitting name for an adventurer!"
      putStrLn ""
      return $ gameState (player name')
    Nothing -> do
      putStrLn "Please enter a valid name"
      putStrLn ""
      createCharacter gameState


-- Core encounter
beginEncounter :: GameState -> IO ()
beginEncounter gameState = do 
  putStrLn $ "You have encountered an " ++ (getMonsterName $ getMonster gameState)
  putStrLn "You can [Attack] or [Flee]. What do you do?" 
  showPlayerHP gameState 
  userChoice <- getLine
  case userChoice of 
    "attack" -> do 
      putStrLn "You hit the orc for 1 damage!"
      putStrLn "The orc hits you for 1 damage!" 
      newGameState <- damageCalculation gameState  
      putStrLn ""
      beginEncounter newGameState
    "flee"   -> do 
      putStrLn "You have fled"
      putStrLn ""
      beginEncounter gameState 

showPlayerHP :: GameState -> IO ()
showPlayerHP gameState = 
  putStr $ 
    "[" ++ "HP" ++ " " ++ 
    show (getPlayerHP $ getPlayer gameState) 
    ++ "/10" ++ "]" ++ ">" ++ " "


damageCalculation :: GameState -> IO GameState 
damageCalculation (GameState player monster) = 
  return $ GameState 
    (Player (getPlayerName player) (getPlayerHP player - 1)) (Monster (getMonsterName monster) (getMonsterHP monster - 1))