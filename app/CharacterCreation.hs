module CharacterCreation where 

data Player =
  Player { getPlayerName :: String,
           getPlayerHP :: Int
           }

getName :: IO (Maybe String)
getName = do 
  name <- getLine 
  case name of 
    "" -> return Nothing
    xs -> return $ Just xs

player :: String -> Player 
player name = Player name 10 
