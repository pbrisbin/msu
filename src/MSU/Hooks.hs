module MSU.Hooks
    ( runHook
    )
where

import Control.Monad
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath ((</>))

runHook :: (String -> IO ()) -> String -> IO ()
runHook run h = do
    hookFile <- findHookFile h
    isExec <- isExecutable hookFile
    when isExec $ run hookFile

findHookFile :: String -> IO FilePath
findHookFile h = (</> h) <$> getUserConfigDir "msu"

isExecutable :: FilePath -> IO Bool
isExecutable fp = do
    exists <- doesFileExist fp
    if exists then executable <$> getPermissions fp else pure False
