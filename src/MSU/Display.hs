module MSU.Display
    ( Mode(..)
    , Display(..)
    )
where

import Data.List (partition)

data Mode = Mode Int Int

instance Show Mode where
    show (Mode w h) = show w ++ "x" ++ show h

data Display = Display
    { name :: String
    , modes :: [Mode]
    }
