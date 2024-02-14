{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( encodeStrict,
    decodeMaybe
  )
where

import Data.Binary
import qualified Data.ByteString as BS

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: Binary a => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)
