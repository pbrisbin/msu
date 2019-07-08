module MSU.Xrandr.Parse
    ( parseXrandr
    )
where

import Control.Monad (void)
import MSU.Display
import Text.Parsec
import Text.Parsec.String

parseXrandr :: String -> Either ParseError [Display]
parseXrandr = parse parseDisplays "xrandr --query"

parseDisplays :: Parser [Display]
parseDisplays = string "Screen" *> ignoreLine *> manyTill parseDisplay eof

parseDisplay :: Parser Display
parseDisplay =
    Display <$> manyTill anyToken space <*> parseModeLinesIfConnected

parseModeLinesIfConnected :: Parser [Mode]
parseModeLinesIfConnected = do
    c <- parseConnected <* ignoreLine
    if c then parseModeLines else skipModeLines

parseConnected :: Parser Bool
parseConnected = True <$ string "connected" <|> False <$ string "disconnected"

parseModeLines :: Parser [Mode]
parseModeLines = manyTill parseModeLine nextDisplay

skipModeLines :: Parser [Mode]
skipModeLines = [] <$ ignoreLinesTill nextDisplay

parseModeLine :: Parser Mode
parseModeLine =
    Mode
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
