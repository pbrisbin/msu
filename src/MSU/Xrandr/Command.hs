module MSU.Xrandr.Command
    ( Xrandr
    , buildCommand
    , defaultCommand
    )
where

import Control.Monad
import Control.Monad.Writer
import MSU.Display

type Xrandr a = Writer String a

defaultCommand :: [Display] -> Xrandr ()
defaultCommand displays = do
    allOff displays
    firstOn displays
    extend rightOf displays

output :: Display -> Xrandr ()
output d = tell $ " --output " ++ name d

off :: Xrandr ()
off = tell " --off"

mode :: Mode -> Xrandr ()
mode m = tell $ " --mode " ++ show m

rightOf :: Display -> Xrandr ()
rightOf d = tell $ " --right-of " ++ name d

outputOff :: Display -> Xrandr ()
outputOff d = output d >> off

-- | Return @False@ if the output was not connected.
outputOn :: Display -> Xrandr Bool
outputOn (Display _ []) = pure False
outputOn d@(Display _ (m : _)) = output d >> mode m >> pure True

allOff :: [Display] -> Xrandr ()
allOff = mapM_ outputOff

firstOn :: [Display] -> Xrandr ()
firstOn = foldr ((||>) . outputOn) (pure ())

extend :: (Display -> Xrandr ()) -> [Display] -> Xrandr ()
extend f = fold1M_ (placeWith f)

placeWith :: (Display -> Xrandr ()) -> Display -> Display -> Xrandr Display
placeWith placementCmd primary secondary = do
    didTurnOn <- outputOn secondary
    if didTurnOn then secondary <$ placementCmd primary else pure primary

buildCommand :: Xrandr a -> String
buildCommand f = execWriter $ tell "xrandr" >> f

-- | Executes the second action IFF the first returns @False@.
(||>) :: Monad m => m Bool -> m () -> m ()
f ||> g = f >>= \b -> unless b g

-- | Like foldM_ but uses first element as base value
fold1M_ :: Monad m => (b -> b -> m b) -> [b] -> m ()
fold1M_ _ [] = pure ()
fold1M_ f (x : xs) = foldM_ f x xs
