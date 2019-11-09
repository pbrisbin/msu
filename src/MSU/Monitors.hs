{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MSU.Monitors
    ( Monitors(..)
    , readMonitorsFileThrow
    , readMonitorsYaml
    , findMonitors

    -- * Create
    , createMonitor
    , writeMonitorsFile
    )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import MSU.Context (Context)
import qualified MSU.Context as Context
import MSU.Match
import qualified MSU.Xrandr.Parse as Xrandr
import System.Directory (doesFileExist)

data Monitors = Monitors
    { name :: String
    , match :: MatchContext
    , exec :: String
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

data MatchContext = MatchContext
    { displays :: Maybe (Match [String])
    , wifi :: Maybe (Match String)
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

readMonitorsFileThrow :: MonadIO m => FilePath -> m [Monitors]
readMonitorsFileThrow = Yaml.decodeFileThrow

readMonitorsYaml :: ByteString -> Either String [Monitors]
readMonitorsYaml = first show . Yaml.decodeEither'

findMonitors :: Context -> [Monitors] -> Maybe Monitors
findMonitors context = find $ \Monitors {..} ->
    let
        connected = filter Xrandr.connected $ Context.displays context
        names = map Xrandr.name connected
        mEssid = Context.essid <$> Context.wifi context
    in displays match `matches` names && wifi match `matchesMaybe` mEssid

createMonitor :: Context -> Monitors
createMonitor context = Monitors
    { name = "created"
    , match = MatchContext
        { displays = Just $ Eq connectedNames
        , wifi = Eq . Context.essid <$> Context.wifi context
        }
    , exec = unwords $ ("xrandr" :) $ concatMap xrandrArg $ Context.displays
        context
    }
  where
    connectedNames =
        map Xrandr.name $ filter Xrandr.connected $ Context.displays context

    xrandrArg display =
        ["--output", Xrandr.name display]
            <> if Xrandr.connected display then xrandrMode display else []

    xrandrMode display = case Xrandr.modes display of
        ((x, y) : _) -> ["--mode", show x <> "x" <> show y]
        _ -> []

writeMonitorsFile :: MonadIO m => FilePath -> Monitors -> m ()
writeMonitorsFile path monitors = liftIO $ do
    exists <- doesFileExist path
    if exists
        then BS.appendFile path $ "\n" <> encoded
        else BS.writeFile path encoded
    where encoded = Yaml.encode [monitors]
