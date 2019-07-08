module Main (main) where

import Control.Monad (void)
import MSU.Hooks
import MSU.Xrandr.Command
import MSU.Xrandr.Parse
import System.IO (hPrint, stderr)
import System.Process (callCommand, readProcess)

main :: IO ()
main = do
    xrandrOutput <- readProcess "xrandr" ["--query"] ""

    case parseXrandr xrandrOutput of
        Left err -> hPrint stderr err
        Right displays -> do
            runCmd $ buildCommand $ defaultCommand displays
            runHook runCmd "after-setup"

runCmd :: String -> IO ()
runCmd cmd = do
    putStrLn cmd
    void $ callCommand cmd
