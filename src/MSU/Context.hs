module MSU.Context
    ( Context(..)
    , getContext
    , Display(..)
    , getDisplays
    , Wifi(..)
    , getWifi
    )
where

import MSU.Xrandr.Parse hiding (Display)
import qualified MSU.Xrandr.Parse as Xrandr
import System.Process (readProcess)
import UnliftIO.Exception (throwString)

data Context = Context
    { cDisplays :: [Display]
    , cWifi :: Maybe Wifi
    }

newtype Display = Display
    { dName :: String
    }

newtype Wifi = Wifi
    { wEssid :: String
    }

getContext :: IO Context
getContext = Context <$> getDisplays <*> getWifi

getDisplays :: IO [Display]
getDisplays = do
    result <- parseXrandr <$> readProcess "xrandr" ["--query"] ""
    either
        (throwString . show)
        (pure . map (Display . Xrandr.name) . filter Xrandr.connected)
        result

-- TODO
getWifi :: IO (Maybe Wifi)
getWifi = pure Nothing
