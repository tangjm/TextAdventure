module Main where

import Data.Char (toLower)
import System.Random 
import CharacterCreation 

main :: IO ()
main = putStrLn "Hello, Haskell!"

data GameState = 
  GameState { getPlayer :: Player, 
              getMonster :: Monster,
              getRandNum :: StdGen }

data Monster = 
  Monster { getMonsterName :: String,
            getMonsterHP :: Int }

type Being = String 

gameFor :: Player -> StdGen -> GameState
gameFor = flip GameState (Monster "Orc" 10) 

start :: IO ()
start = do 
  player <- createCharacter
  let game = gameFor player stdNumGen in 
    beginEncounter game


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
      newGameState <- damageCalculation gameState  
      putStrLn ""
      beginEncounter newGameState
    "flee"   -> do 
      putStrLn "You have fled"
      putStrLn ""
      beginEncounter gameState 

showPlayerHP :: GameState -> IO ()
showPlayerHP (GameState player _ _) = 
  putStr $ 
    "[" ++ "HP" ++ " " ++ 
    show (getPlayerHP player) 
    ++ "/10" ++ "]" ++ ">" ++ " "


damageCalculation :: GameState -> IO GameState 
damageCalculation (GameState player monster randNum) = do 
  let monsterDamage = getDamageFor (getMonsterName monster) randNum 
  let playerDamage = getDamageFor "player" randNum 
  case monsterDamage of 
    (Right (monsterDamage', _)) ->   
      case playerDamage of 
        (Right (playerDamage', newRandNum)) -> do
            putStrLn $ "You hit the orc for " ++ show playerDamage' ++ " damage!"
            putStrLn $ "The orc hits you for " ++ show monsterDamage' ++ " damage!" 
            return $ GameState 
              (Player (getPlayerName player) 
                      (getPlayerHP player - monsterDamage') 
                      (getPlayerRace player)
                      (getPlayerClass player))
              (Monster (getMonsterName monster) 
                      (getMonsterHP monster - playerDamage'))
              newRandNum
        (Left _) -> do 
          putStrLn "Player doesn't exist"
          return $ GameState player monster randNum
    (Left _) -> do 
      putStrLn "Monster doesn't exist" 
      return $ GameState player monster randNum 
    

stdNumGen :: StdGen 
stdNumGen = mkStdGen 5

randomNumInRange :: (Int, Int) -> StdGen -> (Int, StdGen) 
randomNumInRange (min,max) numGen = randomR (min, max) numGen 

getDamageFor :: Being -> StdGen -> Either String (Int, StdGen)
getDamageFor being numGen = do 
  let being' = map toLower being
  case being' of 
    "orc" -> return $ randomNumInRange (0, 10) numGen
    "player" -> return $ randomNumInRange (0, 2) numGen
    _ -> Left "Non-existent being"