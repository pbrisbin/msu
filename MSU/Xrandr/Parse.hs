module MSU.Xrandr.Parse where

import MSU.Display
import Text.Parsec
import Text.Parsec.String

parseXrandr :: String -> Either ParseError [Display]
parseXrandr = parse parseDisplays "xrandr --query"

parseDisplays :: Parser [Display]
parseDisplays = do
    string "Screen" >> ignoreLine

    manyTill parseDisplay eof

parseDisplay :: Parser Display
parseDisplay = try parseConnected <|> parseDisconnected

parseConnected :: Parser Display
parseConnected = do
    name <- manyTill anyToken space
    string "connected" >> ignoreLine
    modes <- manyTill parseModeLine (lookAhead $ try parseDisplay)

    return $ Connected name modes

parseDisconnected :: Parser Display
parseDisconnected = do
    name <- manyTill anyToken space
    string "disconnected" >> ignoreLine

    return $ Disconnected name

parseModeLine :: Parser Mode
parseModeLine = do
    _ <- spaces
    w <- many digit
    _ <- char 'x'
    h <- many digit
    ignoreLine

    return $ Mode (read w) (read h)

ignoreLine :: Parser ()
ignoreLine = manyTill anyToken eol >> return ()
    where
        eol :: Parser Char
        eol = char '\n'
