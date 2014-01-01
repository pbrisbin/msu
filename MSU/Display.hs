module MSU.Display where

data Mode = Mode Int Int

instance Show Mode where
    show (Mode w h) = show w ++ "x" ++ show h

data Display = Display
    { name  :: String
    , modes :: [Mode]
    }

isConnected :: Display -> Bool
isConnected (Display _ []) = False
isConnected _              = True
