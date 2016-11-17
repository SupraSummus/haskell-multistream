{-# LANGUAGE FlexibleContexts #-}

module Data.Bytes.Varint (varintEncode, supplyVarint) where

import Control.Monad.Supply
import Data.Word
import Data.Bits

varintEncode :: Integral a => a -> [Word8]
varintEncode n | n > 127 = (setBit (toEnum $ fromEnum $ n `div` 128) 7):(varintEncode $ n `mod` 128)
               | otherwise = [toEnum $ fromEnum n]

supplyVarint :: (Monad m, Integral a) => (m Word8) -> m a
supplyVarint supply = supplyVarintAcc supply 0

supplyVarintAcc :: (Monad m, Integral a) => (m Word8) -> a -> m a
supplyVarintAcc supply acc = do
    byte <- supply
    let val = (acc * 128) + (toEnum $ fromEnum $ clearBit byte 7)
    if testBit byte 7 then
        supplyVarintAcc supply val
    else
        return val
