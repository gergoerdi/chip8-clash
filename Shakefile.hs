{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "CHIP8"
    , clashModule = "CHIP8"
    , clashTopName = "CHIP8"
    , topName = "Top"
    , ipCores = ["ClockMan25"]
    , vhdlSrcs = ["Top"]
    , clashFlags =
        [ "-iclash-sandbox/lib/src-clash"
        , "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=100"
        ]
    , shakeDir = "clash-sandbox/shake"
    }

main :: IO ()
main = mainFor clashProject
