{-# LANGUAGE RecordWildCards #-}
module SDLIO.Memory
  (Memory, memBuf, mkMemory
  , latchAddress
  , readData
  , writeData
  ) where

import Clash.Prelude

import Data.IORef
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (traverse_)

import Data.Array.IO
import Data.Array.MArray
import qualified Data.List as L

data Memory a d = Memory
    { memAddrReg :: IORef (Maybe a)
    , memBuf :: IOArray a d
    }

mkMemory :: (Ix a) => (a, a) -> [d] -> d -> IO (Memory a d)
mkMemory bounds initial def = do
    memAddrReg <- newIORef Nothing
    memBuf <- newListArray bounds (initial <> L.repeat def)
    return $ Memory{..}

latchAddress :: Memory a d -> a -> IO ()
latchAddress Memory{..} = writeIORef memAddrReg . Just

readData :: (Ix a) => Memory a d -> IO d
readData Memory{..} = do
    addr <- readIORef memAddrReg
    maybe (return $ error "Address register unset") (readArray memBuf) addr

writeData :: (Ix a) => Memory a d -> a -> d -> IO ()
writeData Memory{..} addr d = writeArray memBuf addr d
