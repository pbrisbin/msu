module MSU.Xrandr.Command where

import Control.Monad
import Control.Monad.Writer
import Data.Monoid
import MSU.Display

newtype Command = Command String deriving Show

instance Monoid Command where
    mempty = Command ""
    mappend (Command cmd1) (Command cmd2) = Command $ cmd1 ++ cmd2

type Xrandr a = Writer Command a

output :: Display -> Xrandr ()
output d = tell $ Command $ " --output " ++ name d


off :: Xrandr ()
off = tell $ Command " --off"

mode :: Mode -> Xrandr ()
mode m = tell $ Command $ " --mode " ++ show m

rightOf :: Display -> Xrandr ()
rightOf d = tell $ Command $ " --right-of " ++ name d

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

buildCommand :: (Xrandr a) -> Command
buildCommand f = execWriter $ tell (Command "xrandr") >> f
