module CharacterCreation where 

import System.Random (StdGen)

data Race = 
      Elf
    | Dwarf
    | Orc
    deriving (Eq, Show)

data Class = 
      Archer
    | Mage 
    | Warrior
    deriving (Eq, Show)

data Player =
  Player { getPlayerName :: String,
           getPlayerHP :: Int,
           getPlayerRace :: Race,
           getPlayerClass :: Class
           }
  deriving Show

getName :: IO (Maybe String)
getName = do 
  name <- getLine 
  case name of 
    "" -> return Nothing
    xs -> return $ Just xs

life :: Int 
life = 10


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
