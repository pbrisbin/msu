module MSU.Context
    ( Context(..)
    , getContext
    , Display(..)
    , getDisplays
    , Wifi(..)
    , getWifi
    )
where

import MSU.Xrandr.Parse
import System.Process (readProcess)

data Context = Context
   { displays :: [Display]
   , wifi :: Maybe Wifi
   }

newtype Wifi = Wifi
    { essid :: String
    }

getContext :: IO Context
getContext = Context <$> getDisplays <*> getWifi

getDisplays :: IO [Display]
getDisplays = parseXrandrUnsafe =<< readProcess "xrandr" ["--query"] ""

-- TODO
getWifi :: IO (Maybe Wifi)
getWifi = pure Nothing
