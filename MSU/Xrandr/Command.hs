module MSU.Xrandr.Command where

import Control.Monad
import Control.Monad.Writer
import Data.Monoid
import MSU.Display

type Xrandr a = Writer String a

output :: Display -> Xrandr ()
output d = tell $ " --output " ++ name d

off :: Xrandr ()
off = tell " --off"

mode :: Mode -> Xrandr ()
mode m = tell $ " --mode " ++ show m

rightOf :: Display -> Xrandr ()
rightOf d = tell $ " --right-of " ++ name d

allOff :: [Display] -> Xrandr ()
allOff = mapM_ (\d -> output d >> off)

-- | N.B. Only to be called on connected displays
extendRight :: [Display] -> Xrandr ()
extendRight [] = return ()
extendRight (first:rest) = do
    foldM_ setRight first rest

    where
        setRight :: Display -> Display -> Xrandr Display
        setRight primary secondary = do
            output secondary >> off
            output secondary >> mode (head $ modes secondary)
            rightOf primary

            return secondary

buildCommand :: (Xrandr a) -> String
buildCommand f = execWriter $ tell "xrandr" >> f
