{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module FetchM where

import Prelude ()
import Clash.Prelude hiding (lift)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Default

data Failure
    = Underrun
    | Overrun
    deriving Show

data Buffer n dat = Buffer
    { bufferContents :: Vec n dat
    , bufferLast :: Maybe (Index n)
    }
    deriving (Show, Generic, Undefined)

instance (KnownNat n, Default dat) => Default (Buffer n dat) where
    def = Buffer (pure def) Nothing

remember :: (KnownNat n) => Buffer n dat -> dat -> Buffer n dat
remember Buffer{..} x = Buffer
    { bufferContents = replace bufferLast' x bufferContents
    , bufferLast = Just bufferLast'
    }
  where
    bufferLast' = maybe minBound (+ 1) bufferLast

newtype FetchM n dat m a = FetchM{ unFetchM :: ReaderT (Buffer n dat) (StateT (Maybe (Index n)) (ExceptT Failure m)) a }
    deriving newtype (Functor, Applicative, Monad)

runFetchM :: (Monad m, KnownNat n) => Buffer n dat -> FetchM n dat m a -> m (Either Failure a)
runFetchM buf act = runExceptT $ evalStateT (runReaderT (unFetchM act) buf) Nothing

fetch :: (Monad m, KnownNat n) => FetchM n dat m dat
fetch = do
    Buffer{..} <- FetchM ask
    case bufferLast of
        Nothing -> underrun
        Just bufferLast -> do
            idx <- FetchM get
            when (maybe False (== maxBound) idx) overrun
            when (maybe False (>= bufferLast) idx) underrun
            let idx' = maybe minBound (+ 1) idx
            FetchM $ put $ Just idx'
            return $ bufferContents !! idx'
  where
    overrun = FetchM . lift . lift . throwE $ Overrun
    underrun = FetchM . lift . lift . throwE $ Underrun
