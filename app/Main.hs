module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

data GameState = 
  GameState { getHP :: Int, getMonsterHP :: Int }

gameState :: GameState
gameState = GameState 10 10 

start :: IO ()
start = beginEncounter gameState 

-- Core encounter
beginEncounter :: GameState -> IO ()
beginEncounter gameState = do 
  putStrLn "You have encountered an orc."
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
    show (getHP gameState) 
    ++ "/10" ++ "]" ++ ">" ++ " "


damageCalculation :: GameState -> IO GameState 
damageCalculation (GameState player monster) = 
  return $ GameState (player - 1) (monster - 1)