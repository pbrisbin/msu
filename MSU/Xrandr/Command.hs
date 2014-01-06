module MSU.Xrandr.Command where

import Control.Monad
import Control.Monad.Writer
import MSU.Display

type Xrandr a = Writer String a

output :: Display -> Xrandr ()
output = tell . (" --output " ++) . name

off :: Xrandr ()
off = tell " --off"

mode :: Mode -> Xrandr ()
mode = tell . (" --mode " ++) . show

rightOf :: Display -> Xrandr ()
rightOf = tell . (" --right-of " ++) . name

outputOff :: Display -> Xrandr ()
outputOff d = output d >> off

-- | Does nothing but return @False@ if the output's not connected.
outputOn :: Display -> Xrandr Bool
outputOn (Display _ []) = return False
outputOn d@(Display _ (m:_)) = output d >> mode m >> return True

-- | Some Apple displays freak out if you change the resolution, turning
--   it "off and then on again" seems to avoid that. Sigh.
outputOffOn :: Display -> Xrandr Bool
outputOffOn d = outputOff d >> outputOn d

allOff :: [Display] -> Xrandr ()
allOff = mapM_ outputOff

firstOn :: [Display] -> Xrandr ()
firstOn []    = return ()
firstOn (d:_) = outputOn d >> return ()

extendRight :: [Display] -> Xrandr ()
extendRight [] = return ()
extendRight (first:rest) = foldM_ setRight first rest

    where
        setRight :: Display -> Display -> Xrandr Display
        setRight primary secondary = do
            outputOffOn secondary &&> rightOf primary

            return secondary

buildCommand :: (Xrandr a) -> String
buildCommand f = execWriter $ tell "xrandr" >> f

-- | Executes the second action IFF the first returns @True@.
(&&>) :: Monad m => m Bool -> m () -> m ()
f &&> g = f >>= \b -> if b then g else return ()
