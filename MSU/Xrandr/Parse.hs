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
parseDisplay = do
    n  <- manyTill anyToken space
    _ <- string "connected" <|> string "disconnected"
    ignoreLine
    ms <- option [] $ parseModeLines

    return $ Display n ms

parseModeLines :: Parser [Mode]
parseModeLines = manyTill parseModeLine nextDisplay

    where
        nextDisplay :: Parser Display
        nextDisplay = lookAhead $ try parseDisplay

parseModeLine :: Parser Mode
parseModeLine = do
    w <- spaces >> many digit
    h <- char 'x' >> many digit
    ignoreLine

    return $ Mode (read w) (read h)

ignoreLine :: Parser ()
ignoreLine = manyTill anyToken eol >> return ()

    where
        eol :: Parser Char
        eol = char '\n'
