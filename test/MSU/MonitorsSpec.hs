{-# LANGUAGE OverloadedStrings #-}

module MSU.MonitorsSpec
    ( spec
    )
where

import Test.Hspec

import Control.Error.Util (hush)
import Control.Monad (void)
import Data.ByteString (ByteString)
import MSU.Context
import MSU.Monitors

spec :: Spec
spec = do
    describe "readMonitorsYaml" $ do
        it "parses valid input" $ do
            void (readMonitorsYaml monitorsYaml) `shouldBe` Right ()

    describe "findMonitors" $ do
        it "finds matches" $ do
            let context1 = Context
                    { cDisplays = [ Display {dName = "eDP1"}
                                  , Display {dName = "DP2-2"}
                                  ]
                    , cWifi = Just $ Wifi {wEssid = "pb-and-j"}
                    }
                context2 = Context
                    { cDisplays = [ Display {dName = "eDP1"}
                                  , Display {dName = "DP2-2"}
                                  ]
                    , cWifi = Just $ Wifi {wEssid = "other-wifi"}
                    }
                context3 = Context
                    { cDisplays = [ Display {dName = "eDP1"}
                                  , Display {dName = "DP2-2"}
                                  , Display {dName = "DP2-3"}
                                  ]
                    , cWifi = Just $ Wifi {wEssid = "pb-and-j"}
                    }

                findMonitorsExec c = do
                    ms <- hush $ readMonitorsYaml monitorsYaml
                    mExec <$> findMonitors c ms

            findMonitorsExec context1
                `shouldBe` Just
                               "xrandr --output eDP1 --mode 2560x1440 --right-of DP2-2 --output DP1 --off --output DP2 --off --output DP2-1 --off --output DP2-2 --primary --mode 2560x1440 --output DP2-3 --off --output HDMI1 --off --output HDMI2 --off --output VIRTUAL1 --off"
            findMonitorsExec context2 `shouldBe` Nothing
            findMonitorsExec context3 `shouldBe` Nothing

monitorsYaml :: ByteString
monitorsYaml = mconcat
    [ "- name: none\n"
    , "  match:\n"
    , "    displays:\n"
    , "      connected:\n"
    , "        eq:\n"
    , "          - eDP1\n"
    , "\n"
    , "  exec: \n"
    , "    xrandr\n"
    , "      --output eDP1 --primary --mode 2560x1440\n"
    , "      --output DP1 --off\n"
    , "      --output DP2 --off\n"
    , "      --output DP2-1 --off\n"
    , "      --output DP2-2 --off\n"
    , "      --output DP2-3 --off\n"
    , "      --output HDMI1 --off\n"
    , "      --output HDMI2 --off\n"
    , "      --output VIRTUAL1 --off\n"
    , "\n"
    , "- name: home-dual\n"
    , "  match:\n"
    , "    displays:\n"
    , "      connected:\n"
    , "        eq:\n"
    , "          - eDP1\n"
    , "          - DP2-2\n"
    , "\n"
    , "    wifi:\n"
    , "      essid:\n"
    , "        in:\n"
    , "          - pb-and-j\n"
    , "          - pb-and-j-5g\n"
    , "\n"
    , "  exec: \n"
    , "    xrandr\n"
    , "      --output eDP1 --mode 2560x1440 --right-of DP2-2\n"
    , "      --output DP1 --off\n"
    , "      --output DP2 --off\n"
    , "      --output DP2-1 --off\n"
    , "      --output DP2-2 --primary --mode 2560x1440\n"
    , "      --output DP2-3 --off\n"
    , "      --output HDMI1 --off\n"
    , "      --output HDMI2 --off\n"
    , "      --output VIRTUAL1 --off\n"
    ]
