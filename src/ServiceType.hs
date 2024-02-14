module ServiceType (
    ServiceType(..),
    serviceCost
) where


import Data.Binary (Binary(..), Word8, Get)

data ServiceType
  = Coins Double
  | Message String
  | Both (Double, String)
  deriving (Show, Eq)

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

serviceCost :: ServiceType -> Double
serviceCost (Coins amount) = amount + amount * 3 / 100 -- 3 % fee
serviceCost (Message mess) = amount + amount * 3 / 100 -- TODO: not sure exactly when the fee applies
  where
    amount = fromIntegral $ Prelude.length mess
serviceCost (Both (amount, mess)) = total + total * 3 / 100
  where
    total = amount + fromIntegral (Prelude.length mess)

