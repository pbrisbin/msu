{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MSU.Match
    ( Match(..)
    , matches
    , matchesMaybe
    )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Maybe (fromMaybe)

data Match a
    = Any
    -- ^ Always matches
    | Eq a
    -- ^ Matches if equal
    | In [a]
    -- ^ Matches if element

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

-- | Apply a @'Match'@ to get @'Bool'@
--
-- Accepts @'Maybe'@ purefuly for convenience of current use (where matches are
-- optional keys in JSON).
--
matches :: Eq a => Maybe (Match a) -> a -> Bool
matches = matchFn . fromMaybe Any

-- | Apply a @'Match'@ to a @'Maybe'@ to get @'Bool'@
--
-- A @'Nothing'@ always matches.
--
matchesMaybe :: Eq a => Maybe (Match a) -> Maybe a -> Bool
matchesMaybe mm = maybe True (mm `matches`)

matchFn :: Eq a => Match a -> a -> Bool
matchFn = \case
    Any -> const True
    Eq a -> (== a)
    In as -> (`elem` as)
