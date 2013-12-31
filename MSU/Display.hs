module MSU.Display where

data Mode = Mode
    { width  :: Int
    , height :: Int
    }

instance Show Mode where
    show (Mode w h) = show w ++ "x" ++ show h

data Display = Connected    { name :: String, modes ::  [Mode] }
             | Disconnected { name :: String }

instance Show Display where
    show (Connected n ms) = n ++ ": " ++ show ms
    show (Disconnected n) = n ++ ": (disconnected)"

isConnected :: Display -> Bool
isConnected (Connected _ _) = True
isConnected _ = False
