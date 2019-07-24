{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    )
where

import MSU.Context
import MSU.Monitors
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)

main :: IO ()
main = do
    yaml <- (</> ".monitors.yaml") <$> getHomeDirectory
    monitors <- findMonitors <$> getContext <*> readMonitorsFileThrow yaml

    maybe
        (putStrLn "No monitors rules matched")
        (\Monitors {..} -> do
            putStrLn mExec
            callCommand mExec
        )
        monitors
