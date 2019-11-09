module MSU.Context
    ( Context(..)
    , getContext
    , Display(..)
    , getDisplays
    , Wifi(..)
    , getWifi
    )
where

import Data.Char (isSpace)
import MSU.Xrandr.Parse
import System.Process (readProcess)
import UnliftIO.Exception (handleAny)

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

getWifi :: IO (Maybe Wifi)
getWifi = do
    x <- getEssid
    pure $ if null x then Nothing else Just $ Wifi { essid = x }

-- TODO: do this "correctly", whatever that would be
getEssid :: IO String
getEssid = handleAny (\_ -> pure "") $ trim <$> readProcess
    "sh"
    ["-c", "iwconfig 2>/dev/null | " <> sed]
    ""
  where
    sed = "sed '/^.* ESSID:\"\\([^\\\"]*\\)\".*$/!d; s//\\1/'"

    trim :: String -> String
    trim = f . f where f = dropWhile isSpace . reverse
