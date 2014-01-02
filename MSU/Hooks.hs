module MSU.Hooks where

import Control.Monad
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath ((</>))

runHook :: (String -> IO ()) -> String -> IO ()
runHook run h = do
    hookFile <- findHookFile h
    isExec   <- isExecutable hookFile

    when isExec $ run hookFile

findHookFile :: String -> IO FilePath
findHookFile h = do
    dir <- getUserConfigDir "msu"

    return $ dir </> h

isExecutable :: FilePath -> IO Bool
isExecutable fp = do
    exists <- doesFileExist fp

    if exists
        then return . executable =<< getPermissions fp
        else return False
