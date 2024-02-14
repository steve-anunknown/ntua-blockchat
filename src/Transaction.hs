{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Transaction
  ( ServiceType (..),
    Transaction (..),
    transactionCost,
    initializeTransaction,
    createTransaction,
    broadcastTransaction,
    verifySignature,
    validateTransaction,
  )
where

import ServiceType 
import Account (Account (..), availableBalance)
import Codec.Crypto.RSA (PrivateKey, PublicKey (..), sign, verify)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Binary
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

---------------------------------------------------------
-- PublicKey is not an instance of Ord, so we make it one
-- in order to be able to index Maps using it.
instance Ord PublicKey where
  (<=) PublicKey {public_n = pn1} PublicKey {public_n = pn2} = pn1 <= pn2

-- This is perhaps bad design, I don't know.
---------------------------------------------------------

data TransactionInit = TransactionInit
  { initSenderAddress :: PublicKey, -- public key of the wallet which the transaction comes from
    initReceiverAddress :: PublicKey, -- public key of the wallet which the transaction sends to
    initServiceType :: ServiceType, -- type of the transaction
    initNonce :: Int -- counter
  }
  deriving (Show)

initializeTransaction :: PublicKey -> PublicKey -> ServiceType -> Int -> TransactionInit
initializeTransaction = TransactionInit -- for user

instance Binary TransactionInit where
  put (TransactionInit senderAddr receiverAddr transType n) = do
    put senderAddr
    put receiverAddr
    put transType
    put n
  get = do
    senderAddr <- get
    receiverAddr <- get
    transType <- get
    TransactionInit senderAddr receiverAddr transType <$> get

data Transaction = Transaction
  { senderAddress :: PublicKey, -- public key of the wallet which the transaction comes from
    receiverAddress :: PublicKey, -- public key of the wallet which the transaction sends to
    serviceType :: ServiceType, -- type of the transaction
    nonce :: Int, -- counter
    hashID :: ByteString,
    signature :: ByteString
  } deriving (Show, Eq)
instance Binary Transaction where -- Transaction is made an instance of Binary for the hash of the block
  put (Transaction senderAddr receiverAddr transType n h s) = do
    put senderAddr
    put receiverAddr
    put transType
    put n
    put h
    put s
  get = do
    senderAddr <- get
    receiverAddr <- get
    transType <- get
    n <- get
    h <- get
    Transaction senderAddr receiverAddr transType n h <$> get

transactionCost :: Transaction -> Double
transactionCost = serviceCost . serviceType

computeHashID :: TransactionInit -> ByteString
computeHashID = convert . hashWith SHA256 . B.toStrict . encode

computeSignature :: PrivateKey -> ByteString -> ByteString
computeSignature privkey bytestring = B.toStrict $ sign privkey (B.fromStrict bytestring)

finalizeTransaction :: TransactionInit -> PrivateKey -> Transaction
finalizeTransaction initTx privKey =
  Transaction
    { senderAddress = initSenderAddress initTx,
      receiverAddress = initReceiverAddress initTx,
      serviceType = initServiceType initTx,
      nonce = initNonce initTx,
      hashID = computeHashID initTx,
      signature = computeSignature privKey (computeHashID initTx)
    }

createTransaction :: PublicKey -> PublicKey -> ServiceType -> Int -> PrivateKey -> Transaction
createTransaction p1 p2 s n = finalizeTransaction (initializeTransaction p1 p2 s n)

-- TODO: implement "broadcastTransaction"
broadcastTransaction :: a -> a
broadcastTransaction _ = error "Not implemented"

verifySignature :: Transaction -> Bool
verifySignature t = verify from sig tid
  where
    from = senderAddress t
    sig = B.fromStrict $ signature t
    tid = B.fromStrict $ hashID t

-- PublicKey has been made an instance of Ord, therefore it can be used
-- as a key in a map.
validateTransaction :: Transaction -> Map.Map PublicKey Account -> Bool
validateTransaction t m = verifySignature t && maybe False validateSender senderAcc
  where
    validateSender acc = availableBalance acc >= transactionCost t
    senderAcc = Map.lookup (senderAddress t) m

