{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Except
import Control.Monad.Supply
import Control.Monad.Writer
import Data.Word
import qualified Data.Map.Strict as Map
import qualified Codec.Binary.UTF8.String as UTF8
import Network.Protocol.Multistream


instance MonadSupply s m => MonadSupply s (ExceptT e m) where
    supply = lift supply
    peek = lift peek
    exhausted = lift exhausted

type Multistream a = ExceptT String (WriterT [Word8] (Supply Word8)) a

main :: IO ()
main = do
    putStrLn "hello"
    putStrLn $ show $ communicate server client

--                                 input                        output   unconsumed input
runMultistream :: Multistream a -> [Word8] -> (Either String a, [Word8], [Word8])
runMultistream multistream input = (result, output, unconsumedInput) where
    -- noError :: WriterT [Word8] (Supply Word8) (Either String a)
    noError = runExceptT multistream
    -- noWriter :: Supply Word8 (Either String a, [Word8])
    noWriter = runWriterT noError
    -- noSupply :: ((Either String a, [Word8]), [Word8])
    noSupply = runSupply noWriter input
    
    noSupplyResult = fst noSupply
    unconsumedInput = snd noSupply
    result = fst noSupplyResult
    output = snd noSupplyResult

--                                                A result                             B result
communicate :: Multistream a -> Multistream b -> ((Either String a, [Word8], [Word8]), (Either String b, [Word8], [Word8]))
communicate a b = (aResult, bResult) where
    aResult = runMultistream a bOutput
    bResult = runMultistream b aOutput
    (_, aOutput, _) = aResult
    (_, bOutput, _) = bResult

server :: Multistream ()
server = Network.Protocol.Multistream.listen (Map.fromList [
        (["const", "OHamburgefonsz"], tell $ UTF8.encode "OHamburgefonsz"),
        (["echo", "1"], while ((return . not) =<< exhausted) $ do {b <- supply; tell [b]}),
        (["echo", "2"], while ((return . not) =<< exhausted) $ do {b <- supply; tell [b, b]})
    ])

client :: Multistream ()
client = do
    dial
    selectProtocol ["echo", "2"]
    tell $ UTF8.encode "hello there"

while :: Monad m => m Bool -> m () -> m ()
while cond stm = do
    b <- cond
    when b $ do
        stm
        while cond stm
