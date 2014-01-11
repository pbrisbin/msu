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
    n <- manyTill anyToken space
    c <- string "connected" <|> string "disconnected"
    ignoreLine

    ms <- if c == "connected"
            then parseModeLines
            else skipModeLines

    return $ Display n ms

parseModeLines :: Parser [Mode]
parseModeLines = manyTill parseModeLine nextDisplay

skipModeLines :: Parser [Mode]
skipModeLines = ignoreLinesTill nextDisplay >> return []

parseModeLine :: Parser Mode
parseModeLine = do
    w <- spaces >> many digit
    h <- char 'x' >> many digit
    ignoreLine

    return $ Mode (read w) (read h)

nextDisplay :: Parser ()
nextDisplay = lookAhead $ try parseDisplay >> return ()

ignoreLinesTill :: Parser () -> Parser ()
ignoreLinesTill p = do
    _ <- manyTill ignoreLine $ p <|> eof
    return ()

ignoreLine :: Parser ()
ignoreLine = manyTill anyToken eol >> return ()

    where
        eol :: Parser Char
        eol = char '\n'
