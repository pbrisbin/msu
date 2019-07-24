{-# LANGUAGE RecordWildCards #-}

module MSU.Xrandr.Parse
    ( Display(..)
    , parseXrandr
    )
where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

data Display = Display
    { name :: String
    , connected :: Bool
    , modes :: [(Int, Int)]
    }
    deriving (Eq, Show)

parseXrandr :: String -> Either ParseError [Display]
parseXrandr = parse parseDisplays "xrandr --query"

parseDisplays :: Parser [Display]
parseDisplays = string "Screen" *> ignoreLine *> manyTill parseDisplay eof

parseDisplay :: Parser Display
parseDisplay = do
    name <- manyTill anyToken space
    connected <- parseConnected <* ignoreLine
    modes <- if connected then parseModeLines else skipModeLines
    pure Display {..}

parseConnected :: Parser Bool
parseConnected = True <$ string "connected" <|> False <$ string "disconnected"

parseModeLines :: Parser [(Int, Int)]
parseModeLines = manyTill parseModeLine nextDisplay

skipModeLines :: Parser [(Int, Int)]
skipModeLines = [] <$ ignoreLinesTill nextDisplay

parseModeLine :: Parser (Int, Int)
parseModeLine =
    (,)
        <$> (read <$> (spaces *> many digit))
        <*> (read <$> (char 'x' *> many digit <* ignoreLine))

nextDisplay :: Parser ()
nextDisplay = lookAhead $ void $ try parseDisplay

ignoreLinesTill :: Parser () -> Parser ()
ignoreLinesTill p = void $ manyTill ignoreLine $ p <|> eof

ignoreLine :: Parser ()
ignoreLine = void $ manyTill anyToken eol
  where
    eol :: Parser Char
    eol = char '\n'
