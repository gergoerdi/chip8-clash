{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import ShakeClash

import Development.Shake hiding ((~>))
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Clash.Prelude
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Maybe (fromMaybe)

clashProject = ClashProject
    { projectName = "CHIP8"
    , clashModule = "CHIP8"
    , clashTopName = "CHIP8"
    , topName = "Top"
    , clashFlags =
        [ "-iclash-utils/src-clash"
        , "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=30"
        , "-fclash-intwidth=32"
        ]
    , shakeDir = "clash-utils/shake"
    , extraGenerated = \ClashKit{..} -> [buildDir </> "image.hex"]
    }

main :: IO ()
main = mainForCustom clashProject $ \ClashKit{..} -> do
    buildDir </> "image.hex" %> \out -> do
        imageFile <- fromMaybe "games/hidden.ch8" <$> getConfig "IMAGE"

        bs <- liftIO $ BS.unpack <$> BS.readFile imageFile
        bs <- return $ L.take 4096 $ L.replicate 0x200 0 <> bs <> L.repeat 0
        let bvs = L.map (filter (/= '_') . show . pack) bs
        writeFileChanged out (unlines bvs)

    phony "model" $ do
        clash "clashi" ["-isrc-model", "src-model" </> "SDLIO.hs"]
