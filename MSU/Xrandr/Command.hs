module MSU.Xrandr.Command where

import Control.Monad
import Control.Monad.Writer
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

outputOff :: Display -> Xrandr ()
outputOff d = output d >> off

-- | Return @False@ if the output was not connected.
outputOn :: Display -> Xrandr Bool
outputOn   (Display _ []   ) = return False
outputOn d@(Display _ (m:_)) = output d >> mode m >> return True

allOff :: [Display] -> Xrandr ()
allOff = mapM_ outputOff

firstOn :: [Display] -> Xrandr ()
firstOn []    = return ()
firstOn (d:_) = outputOn d >> return ()

extendRight :: [Display] -> Xrandr ()
extendRight = fold1M_ (placeWith rightOf)

placeWith :: (Display -> Xrandr ()) -> Display -> Display -> Xrandr Display
placeWith placementCmd primary secondary = do
    outputOff secondary
    outputOn secondary &&> placementCmd primary

    return secondary

buildCommand :: (Xrandr a) -> String
buildCommand f = execWriter $ tell "xrandr" >> f

-- | Executes the second action IFF the first returns @True@.
(&&>) :: Monad m => m Bool -> m () -> m ()
f &&> g = f >>= \b -> if b then g else return ()

-- | Like foldM_ but uses first element as base value
fold1M_ :: Monad m => (b -> b -> m b) -> [b] -> m ()
fold1M_ _ [] = return ()
fold1M_ f (x:xs) = foldM_ f x xs
