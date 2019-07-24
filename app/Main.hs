{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    )
where

import MSU.Context
import MSU.Monitors
import System.Process (callCommand)

main :: IO ()
main = do
    let path = "/home/patrick/.monitors.yaml"
    monitors <- findMonitors <$> getContext <*> readMonitorsFileThrow path

    maybe
        (putStrLn "No monitors rules matched")
        (\Monitors {..} -> do
            putStrLn mExec
            callCommand mExec
        )
        monitors
