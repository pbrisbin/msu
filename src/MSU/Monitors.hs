{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MSU.Monitors
    ( Monitors(..)
    , MonitorsMatch(..)
    , DisplaysMatch(..)
    , WifiMatch(..)
    , writeMonitorsFile
    , readMonitorsFileThrow
    , readMonitorsYaml
    , findMonitors
    , inferMonitor
    )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import MSU.Context
import System.Directory (doesFileExist)

data Monitors = Monitors
    { mName :: String
    -- ^ Name for this Monitors setup
    , mMatch :: MonitorsMatch
    -- ^ How to match a @'Context'@
    --
    -- The first setup to match will be used
    --
    , mExec :: String
    -- ^ Command to execute if matched
    --
    -- Almost certainly some @xrandr@ invocation.
    --
    }
    deriving Generic

instance FromJSON Monitors where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Monitors where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

data MonitorsMatch = MonitorsMatch
    { mmDisplays :: DisplaysMatch
    , mmWifi :: WifiMatch
    }
    deriving Generic

instance FromJSON MonitorsMatch where
    parseJSON = withObject "context predicate" $ \o ->
        MonitorsMatch
            <$> o
            .:? "displays"
            .!= DisplaysMatch Any
            <*> o
            .:? "wifi"
            .!= WifiMatch Any

instance ToJSON MonitorsMatch where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

newtype DisplaysMatch = DisplaysMatch
    { dmConnected :: Match [String]
    }
    deriving Generic

instance FromJSON DisplaysMatch where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON DisplaysMatch where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

newtype WifiMatch = WifiMatch
    { wmEssid :: Match String
    }
    deriving Generic

instance FromJSON WifiMatch where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON WifiMatch where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

data Match a
    = Any -- ^ Always matches
    | Eq a -- ^ Matches if equal
    | In [a] -- ^ Matches if element

instance FromJSON a => FromJSON (Match a) where
    parseJSON = withObject "match operator" $ \o -> do
        mEq <- o .:? "eq"
        mIn <- o .:? "in"
        pure $ fromMaybe Any $ (Eq <$> mEq) <|> (In <$> mIn)

instance ToJSON a => ToJSON (Match a) where
    toJSON = \case
        Any -> Null
        Eq a -> object ["eq" .= a]
        In as -> object ["in" .= as]

matches :: Eq a => Match a -> a -> Bool
matches Any _ = True
matches (Eq a) b = a == b
matches (In as) b = b `elem` as

writeMonitorsFile :: MonadIO m => FilePath -> Monitors -> m ()
writeMonitorsFile path monitors = liftIO $ do
    exists <- doesFileExist path
    let write = if exists then BS.appendFile path else BS.writeFile path
    write $ Yaml.encode [monitors]

-- | Read the @monitors.yaml@ file
readMonitorsFileThrow :: MonadIO m => FilePath -> m [Monitors]
readMonitorsFileThrow = Yaml.decodeFileThrow

-- | Extraced for pure testing
readMonitorsYaml :: ByteString -> Either String [Monitors]
readMonitorsYaml = first show . Yaml.decodeEither'

-- | Return the first @'Monitors'@ to match the given @'Context'@
findMonitors :: Context -> [Monitors] -> Maybe Monitors
findMonitors Context {..} = find $ \Monitors {..} ->
    connectedMatches mMatch cDisplays && maybe True (essidMatches mMatch) cWifi
  where
    connectedMatches MonitorsMatch {..} =
        (dmConnected mmDisplays `matches`) . map dName
    essidMatches MonitorsMatch {..} = (wmEssid mmWifi `matches`) . wEssid

inferMonitor :: Context -> Monitors
inferMonitor Context {..} = Monitors
    { mName = "TODO"
    , mMatch = MonitorsMatch
        { mmDisplays = DisplaysMatch $ Eq $ map dName cDisplays
        , mmWifi = WifiMatch $ maybe Any (Eq . wEssid) cWifi
        }
    , mExec =
        unwords
        $ "xrandar"
        : concatMap (\d -> ["--output", dName d, "--off"]) cDisplays
    }
