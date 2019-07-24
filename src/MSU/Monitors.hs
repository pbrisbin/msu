{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MSU.Monitors
    ( Monitors(..)
    , MonitorsMatch(..)
    , DisplaysMatch(..)
    , WifiMatch(..)
    , readMonitorsFileThrow
    , readMonitorsYaml
    , findMonitors
    )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Casing
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import MSU.Context

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

data MonitorsMatch = MonitorsMatch
    { mmDisplays :: DisplaysMatch
    , mmWifi :: WifiMatch
    }
    deriving Generic

instance FromJSON MonitorsMatch where
    parseJSON = withObject "context predicate" $ \o -> MonitorsMatch
        <$> o .:? "displays" .!= DisplaysMatch Any
        <*> o .:? "wifi" .!= WifiMatch Any

newtype DisplaysMatch = DisplaysMatch
    { dmConnected :: Match [String]
    }
    deriving Generic

instance FromJSON DisplaysMatch where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype WifiMatch = WifiMatch
    { wmEssid :: Match String
    }
    deriving Generic

instance FromJSON WifiMatch where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Match a
    = Any -- ^ Always matches
    | Eq a -- ^ Matches if equal
    | In [a] -- ^ Matches if element

instance FromJSON a => FromJSON (Match a) where
    parseJSON = withObject "match operator" $ \o -> do
        mEq <- o .:? "eq"
        mIn <- o .:? "in"
        pure $ fromMaybe Any $ (Eq <$> mEq) <|> (In <$> mIn)

matches :: Eq a => Match a -> a -> Bool
matches Any _ = True
matches (Eq a) b = a == b
matches (In as) b = b `elem` as

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
