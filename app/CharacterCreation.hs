module CharacterCreation where 

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