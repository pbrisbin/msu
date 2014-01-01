module Main where

import MSU.Display
import MSU.Xrandr.Command
import MSU.Xrandr.Parse
import System.Process (readProcess)

main :: IO ()
main = do
    xrandrOutput <- readProcess "xrandr" ["--query"] ""

    either print handle $ parseXrandr xrandrOutput

handle :: [Display] -> IO ()
handle displays = print $ buildCommand $ do
    allOff $ filter (not . isConnected) displays
    extendRight $ filter isConnected displays
