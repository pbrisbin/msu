module Main where

import Data.List (partition)
import MSU.Display
import MSU.Hooks
import MSU.Xrandr.Command
import MSU.Xrandr.Parse
import System.IO (hPrint, stderr)
import System.Process (readProcess, runCommand)

main :: IO ()
main = do
    xrandrOutput <- readProcess "xrandr" ["--query"] ""

    case parseXrandr xrandrOutput of
        Left err -> hPrint stderr err
        Right displays -> do
            runCmd $ defaultCommand displays
            runHook runCmd "after-setup"

defaultCommand :: [Display] -> String
defaultCommand displays = buildCommand $ do
    let (connected, disconnected) = partition isConnected displays

    allOff disconnected
    firstOn connected
    extendRight connected

runCmd :: String -> IO ()
runCmd cmd = putStrLn cmd >> runCommand cmd >> return ()
