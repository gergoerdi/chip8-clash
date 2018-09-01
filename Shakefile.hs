{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

clashProject = ClashProject
    { projectName = "CHIP8"
    , clashModule = "CHIP8"
    , clashTopName = "CHIP8"
    , topName = "Top"
    , ipCores = ["ClockMan25"]
    , vhdlSrcs = ["Top"]
    , clashFlags = ["-i../clash-sandbox/lib/src-clash"]
    , shakeDir = "../clash-sandbox/shake"
    }

main :: IO ()
main = mainFor clashProject
