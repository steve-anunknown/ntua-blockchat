module ServiceType
  ( ServiceType (..),
    serviceCost,
    serviceFee,
  )
where

import Data.Binary (Binary (..), Get, Word8)

data ServiceType
  = Coins Double
  | Message String
  | Both (Double, String)
  deriving (Read, Show, Eq)

instance Binary ServiceType where
  put (Coins amount) = do
    put (0 :: Word8) -- Tag for Coins
    put amount
  put (Message msg) = do
    put (1 :: Word8) -- Tag for Message
    put msg
  put (Both (amount, msg)) = do
    put (2 :: Word8) -- Tag for Both
    put amount
    put msg
  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> Coins <$> get
      1 -> Message <$> get
      2 -> Both <$> get
      _ -> fail "Invalid ServiceType tag"

serviceFee :: Double
serviceFee = 3 / 100

serviceCost :: ServiceType -> Double
serviceCost (Coins amount) = amount + amount * serviceFee -- 3 % fee
serviceCost (Message mess) = fromIntegral $ length mess -- 1 coin per character
serviceCost (Both (amount, mess)) = serviceCost (Coins amount) + serviceCost (Message mess)
