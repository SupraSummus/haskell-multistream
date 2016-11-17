{-# LANGUAGE FlexibleContexts #-}

module Network.Protocol.Multistream (
    multistreamVersion,
    Network.Protocol.Multistream.listen,
    dial, listProtocols, selectProtocol,
) where

import qualified Codec.Text.Multipath as Multipath
import Data.Bytes.Varint
import Data.Word
import Data.Maybe
import Data.Char
import Control.Monad.Except
import Control.Monad.Supply
import Control.Monad.Writer.Class
import qualified Data.Map.Strict as Map
import qualified Codec.Binary.UTF8.String as UTF8

multistreamVersion = ["multistream", "blabla", "ssdf"]
lsMessage = UTF8.encode "ls"
naMessage = UTF8.encode "na"
newlineChar = head $ UTF8.encode "\n"

listen :: (MonadWriter [Word8] m, MonadSupply Word8 m, MonadError String m) => Map.Map Multipath.Multipath (m a) -> m a
listen protocols = do
    tellProtocol multistreamVersion
    expectProtocol multistreamVersion
    listenCommand protocols

listenCommand :: (MonadWriter [Word8] m, MonadSupply Word8 m, MonadError String m) => Map.Map Multipath.Multipath (m a) -> m a
listenCommand protocols = do
    rawCommand <- supplyLengthPrefixed
    command <- newlineUnterminate rawCommand
    handlerForCommand protocols command

handlerForCommand :: (MonadWriter [Word8] m, MonadSupply Word8 m, MonadError String m) => Map.Map Multipath.Multipath (m a) -> [Word8] -> m a
handlerForCommand protocols command
    | command == lsMessage = do
        tell $ varintEncode $ length $ Map.keys protocols
        mapM_ tellProtocol $ Map.keys protocols
        listenCommand protocols
    | otherwise = either unsupportedProtocol id $ handlerForProtocol command
    where
        handlerForProtocol command = do
            protocolName <- Multipath.fromBytes command
            let maybeHandler = Map.lookup protocolName protocols
            maybe (throwError "unknown protocol") (\handler -> Right $ do
                    tellProtocol protocolName
                    handler
                ) maybeHandler
        unsupportedProtocol error = do
            tellMessage naMessage
            listenCommand protocols

dial :: (MonadWriter [Word8] m, MonadSupply Word8 m, MonadError String m) => m ()
dial = do
    tellProtocol multistreamVersion
    expectProtocol multistreamVersion

listProtocols :: (MonadWriter [Word8] m, MonadSupply Word8 m, MonadError String m) => m [Multipath.Multipath]
listProtocols = do
    tellMessage lsMessage
    supplyProtocols

selectProtocol :: (MonadWriter [Word8] m, MonadSupply Word8 m, MonadError String m) => Multipath.Multipath -> m ()
selectProtocol protocol = do
    tellProtocol protocol
    expectProtocol protocol


lengthPrefixed :: [Word8] -> [Word8]
lengthPrefixed d = varintEncode (length d) ++ d

newlineTerminated :: [Word8] -> [Word8]
newlineTerminated = (++ [newlineChar])

newlineUnterminate :: MonadError String m => [Word8] -> m [Word8]
newlineUnterminate [] = throwError "missing newline"
newlineUnterminate [a] | a == newlineChar = return []
                       | otherwise = throwError "missing newline"
newlineUnterminate (ch:tail) = liftM (ch:) $ newlineUnterminate tail

supplyLengthPrefixed :: (MonadSupply Word8 m, MonadError String m) => m [Word8]
supplyLengthPrefixed = do
    length <- supplyVarint safeSupply
    replicateM length safeSupply

tellProtocol :: (MonadWriter [Word8] m) => Multipath.Multipath -> m ()
tellProtocol = tellMessage . Multipath.toBytes

supplyProtocol :: (MonadSupply Word8 m, MonadError String m) => m Multipath.Multipath
supplyProtocol = do
    bytes <- supplyLengthPrefixed
    nonl <- newlineUnterminate bytes
    Multipath.fromBytes nonl

tellProtocols :: (MonadWriter [Word8] m) => [Multipath.Multipath] -> m ()
tellProtocols protocols = do
    tell $ varintEncode $ length protocols
    mapM_ tellProtocol $ protocols

supplyProtocols :: (MonadSupply Word8 m, MonadError String m) => m [Multipath.Multipath]
supplyProtocols = do
    n <- supplyVarint safeSupply
    replicateM n supplyProtocol

tellMessage :: (MonadWriter [Word8] m) => [Word8] -> m ()
tellMessage = tell . lengthPrefixed . newlineTerminated

expectProtocol :: (MonadSupply Word8 m, MonadError String m) => Multipath.Multipath -> m ()
expectProtocol expectedProtocol = do
    protocol <- supplyProtocol
    when (protocol /= expectedProtocol) (throwError $
        "expected protocol " ++ Multipath.toString expectedProtocol ++
        " but got " ++ Multipath.toString protocol)

safeSupply :: (MonadSupply a m, MonadError String m) => m a
safeSupply = do
    e <- exhausted
    when e $ throwError "supply exhausted"
    supply
