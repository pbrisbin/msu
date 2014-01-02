module Main where

import MSU.Display
import MSU.Xrandr.Command
import MSU.Xrandr.Parse
import System.IO (hPrint, stderr)
import System.Process (readProcess)

main :: IO ()
main = do
    xrandrOutput <- readProcess "xrandr" ["--query"] ""

    case parseXrandr xrandrOutput of
        Left err -> hPrint stderr err
        Right displays -> print $ defaultCommand displays

defaultCommand :: [Display] -> String
defaultCommand displays = buildCommand $ do
    allOff $ filter (not . isConnected) displays
    extendRight $ filter isConnected displays
