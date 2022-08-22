module Main where

import CharacterCreation 

main :: IO ()
main = putStrLn "Hello, Haskell!"

data GameState = 
  GameState { getPlayer :: Player, getMonster :: Monster }

data Monster = 
  Monster { getMonsterName :: String, getMonsterHP :: Int }

gameFor :: Player -> GameState
gameFor = flip GameState (Monster "Orc" 10) 

start :: IO ()
start = do 
  player <- createCharacter
  let game = gameFor player in 
    beginEncounter game


pickName :: IO String
pickName = do 
  name <- getName 
  case name of 
    Just name' -> do
      putStrLn "What a fitting name for an adventurer!"
      putStrLn ""
      return name'
    Nothing -> do
      putStrLn "Please enter a valid name"
      putStrLn ""
      pickName

pickClass :: IO Class 
pickClass = do
  class' <- getLine 
  case class' of 
    "warrior" -> do
      putStrLn "Congratulations on becoming a warrior"
      putStrLn "" 
      return Warrior 
    "archer" -> do 
      putStrLn "Congrats on becoming an archer"
      putStrLn ""
      return Archer
    "mage" -> do 
      putStrLn "Congrats on becoming a mage"
      putStrLn ""
      return Mage

pickRace :: IO Race 
pickRace = do 
  race <- getLine
  case race of
    "elf" -> do
      putStrLn "A wise choice!"
      putStrLn ""
      return Elf 
    "dwarf" -> do
      putStrLn "A solid choice!"
      putStrLn ""
      return Dwarf 
    "orc" -> do 
      putStrLn "A respectable choice!"
      putStrLn ""
      return Orc 

-- Character creation
createCharacter :: IO Player
createCharacter = do 
  putStrLn "Welcome to the text adventure!"
  putStrLn "What is your name, adventurer?" 
  name <- pickName

  putStrLn "Select your race: <Elf>, <Dwarf>, <Orc>"
  race <- pickRace

  putStrLn "Select a class: <Warrior>, <Archer>, <Mage>"
  class' <- pickClass 

  return $ Player name life race class'


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
showPlayerHP (GameState player _) = 
  putStr $ 
    "[" ++ "HP" ++ " " ++ 
    show (getPlayerHP player) 
    ++ "/10" ++ "]" ++ ">" ++ " "


damageCalculation :: GameState -> IO GameState 
damageCalculation (GameState player monster) = 
  return $ GameState 
    (Player (getPlayerName player) 
            (getPlayerHP player - 1) 
            (getPlayerRace player)
            (getPlayerClass player))
    (Monster (getMonsterName monster) 
             (getMonsterHP monster - 1))