{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    )
where

import MSU.Context
import MSU.Monitors
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Process (callCommand)

main :: IO ()
main = do
    yaml <- (</> ".monitors.yaml") <$> getHomeDirectory
    args <- getArgs

    case args of
        ["create"] -> do
            writeMonitorsFile yaml . createMonitor =<< getContext
            putStrLn $ "Template monitors rule written, see: " <> yaml

        _ -> do
            monitors <-
                findMonitors <$> getContext <*> readMonitorsFileThrow yaml

            maybe
                (putStrLn "No monitors rules matched")
                (\Monitors {..} -> do
                    putStrLn exec
                    callCommand exec
                )
                monitors
