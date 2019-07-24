module MSU.Xrandr.ParseSpec
    ( spec
    )
where

import Test.Hspec

import MSU.Xrandr.Parse

spec :: Spec
spec = do
    describe "parseXrandr" $ do
        it "parses xrandr --query into Displays" $ do
            parseXrandr xrandrQuery `shouldBe` Right
                [ Display
                    { name = "eDP1"
                    , connected = True
                    , modes = [ (2560, 1440)
                              , (1920, 1440)
                              , (1856, 1392)
                              , (1792, 1344)
                              , (2048, 1152)
                              , (1920, 1200)
                              , (1920, 1080)
                              , (1600, 1200)
                              , (1680, 1050)
                              , (1400, 1050)
                              , (1600, 900)
                              , (1280, 1024)
                              , (1400, 900)
                              , (1280, 960)
                              , (1368, 768)
                              , (1280, 800)
                              , (1280, 720)
                              , (1024, 768)
                              , (1024, 576)
                              , (960, 540)
                              , (800, 600)
                              , (864, 486)
                              , (640, 480)
                              , (720, 405)
                              , (640, 360)
                              ]
                    }
                , Display {name = "DP1", connected = False, modes = []}
                , Display {name = "DP2", connected = False, modes = []}
                , Display {name = "DP2-1", connected = False, modes = []}
                , Display
                    { name = "DP2-2"
                    , connected = True
                    , modes = [ (2560, 1440)
                              , (2048, 1152)
                              , (1920, 1200)
                              , (1920, 1080)
                              , (1600, 1200)
                              , (1680, 1050)
                              , (1280, 1024)
                              , (1200, 960)
                              , (1152, 864)
                              , (1280, 720)
                              , (1024, 768)
                              , (800, 600)
                              , (720, 576)
                              , (720, 480)
                              , (640, 480)
                              , (720, 400)
                              ]
                    }
                , Display {name = "DP2-3", connected = False, modes = []}
                , Display {name = "HDMI1", connected = False, modes = []}
                , Display {name = "HDMI2", connected = False, modes = []}
                , Display {name = "VIRTUAL1", connected = False, modes = []}
                ]

xrandrQuery :: String
xrandrQuery = unlines
    [ "Screen 0: minimum 8 x 8, current 5120 x 1440, maximum 32767 x 32767"
    , "eDP1 connected 2560x1440+2560+0 (normal left inverted right x axis y axis) 310mm x 170mm"
    , "   2560x1440     60.01*+  59.95  "
    , "   1920x1440     60.00  "
    , "   1856x1392     60.01  "
    , "   1792x1344     60.01  "
    , "   2048x1152     60.00    59.90    59.91  "
    , "   1920x1200     59.88    59.95  "
    , "   1920x1080     59.96    60.00    59.93  "
    , "   1600x1200     60.00  "
    , "   1680x1050     59.95    59.88  "
    , "   1400x1050     59.98  "
    , "   1600x900      60.00    59.95    59.82  "
    , "   1280x1024     60.02  "
    , "   1400x900      59.96    59.88  "
    , "   1280x960      60.00  "
    , "   1368x768      60.00    59.88    59.85  "
    , "   1280x800      59.81    59.91  "
    , "   1280x720      59.86    60.00    59.74  "
    , "   1024x768      60.00  "
    , "   1024x576      60.00    59.90    59.82  "
    , "   960x540       60.00    59.63    59.82  "
    , "   800x600       60.32    56.25  "
    , "   864x486       60.00    59.92    59.57  "
    , "   640x480       59.94  "
    , "   720x405       59.51    60.00    58.99  "
    , "   640x360       59.84    59.32    60.00  "
    , "DP1 disconnected (normal left inverted right x axis y axis)"
    , "DP2 disconnected (normal left inverted right x axis y axis)"
    , "DP2-1 disconnected (normal left inverted right x axis y axis)"
    , "DP2-2 connected primary 2560x1440+0+0 (normal left inverted right x axis y axis) 600mm x 340mm"
    , "   2560x1440     59.95*+"
    , "   2048x1152     60.00  "
    , "   1920x1200     59.88  "
    , "   1920x1080     60.00    50.00    59.94    30.00    25.00    24.00    29.97    23.98  "
    , "   1600x1200     60.00  "
    , "   1680x1050     59.95  "
    , "   1280x1024     75.02    60.02  "
    , "   1200x960      59.99  "
    , "   1152x864      75.00  "
    , "   1280x720      60.00    50.00    59.94  "
    , "   1024x768      75.03    60.00  "
    , "   800x600       75.00    60.32  "
    , "   720x576       50.00  "
    , "   720x480       60.00    59.94  "
    , "   640x480       75.00    60.00    59.94  "
    , "   720x400       70.08  "
    , "DP2-3 disconnected (normal left inverted right x axis y axis)"
    , "HDMI1 disconnected (normal left inverted right x axis y axis)"
    , "HDMI2 disconnected (normal left inverted right x axis y axis)"
    , "VIRTUAL1 disconnected (normal left inverted right x axis y axis)"
    , ""
    ]
