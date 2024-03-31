{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( encodeStrict,
    decodeMaybe,
    receiveChunks
  )
where

import Data.Binary ( Binary, decode, encode )
import Network.Simple.TCP ( Socket, recv )
import qualified Data.ByteString as BS

encodeStrict :: (Binary a) => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: (Binary a) => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)

receiveChunks :: Socket -> Int -> IO BS.ByteString
receiveChunks socket limit = receiveChunks' ""
  where
    receiveChunks' acc | BS.length acc < limit = do
      raw <- recv socket 1024 -- have a byte
      case raw of
        Nothing -> return acc -- can't get no more bytes
        Just msg -> receiveChunks' $ BS.append acc msg -- keep eating
    receiveChunks' acc | BS.length acc == limit = return acc
    receiveChunks' acc = return $ fst $ BS.splitAt limit acc
