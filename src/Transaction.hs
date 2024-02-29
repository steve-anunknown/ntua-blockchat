{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Transaction
  ( Transaction (..),
    txCost,
    txsFee,
    createTransaction,
    broadcastTransaction,
    verifySignature,
    validateTransaction,
    updateAccsByTX,
    updateAccsByTXs
  )
where

import Account (Account (..), availableBalance, updateBalanceBy)
import Codec.Crypto.RSA (PrivateKey, PublicKey (..), sign, verify)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Binary
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Network.Simple.TCP
import ServiceType
import Utils

---------------------------------------------------------
-- PublicKey is not an instance of Ord, so we make it one
-- in order to be able to index Maps using it.
instance Ord PublicKey where
  (<=) :: PublicKey -> PublicKey -> Bool
  (<=) PublicKey {public_n = pn1} PublicKey {public_n = pn2} = pn1 <= pn2

-- This is perhaps bad design, I don't know.
---------------------------------------------------------

data TransactionInit = TransactionInit
  { initSenderAddress :: PublicKey, -- pub key which the tx comes from
    initReceiverAddress :: PublicKey, -- pub key which the tx sends to
    initServiceType :: ServiceType, -- type of the tx
    initNonce :: Int -- counter
  }
  deriving (Show)

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
  }
  deriving (Show, Eq)

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

-- this is deducted from the sender's account
txCost :: Transaction -> Double
txCost = serviceCost . serviceType

-- this is used during the minting phase
txsFee :: [Transaction] -> Double
txsFee = sum . map txFee
  where txFee Transaction {serviceType = service} = case service of
          Coins c -> c * serviceFee
          Message msg -> fromIntegral (length msg) * serviceFee
          Both (c , msg) -> c * serviceFee + fromIntegral (length msg) * serviceFee

-- use this to update the state of the accounts, specifically the balances
-- of the sender and the receiver
updateAccsByTX :: Transaction -> Map.Map PublicKey Account -> Map.Map PublicKey Account
updateAccsByTX t m = case serviceType t of
  Coins c -> let cost = - txCost t
                 sender = senderAddress t
                 receiver = receiverAddress t
                 temp = Map.adjust (updateBalanceBy cost) sender m
             in Map.adjust (updateBalanceBy c) receiver temp
  Message _ -> let cost = - txCost t
                   sender = senderAddress t
               in Map.adjust (updateBalanceBy cost) sender m
  Both (c, _) -> let cost = - txCost t
                     sender = senderAddress t
                     receiver = receiverAddress t
                     temp = Map.adjust (updateBalanceBy cost) sender m
                 in Map.adjust (updateBalanceBy c) receiver temp

updateAccsByTXs :: [Transaction] -> Map.Map PublicKey Account -> Map.Map PublicKey Account
updateAccsByTXs txs initial = foldr updateAccsByTX initial txs

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
createTransaction p1 p2 s n = finalizeTransaction (TransactionInit p1 p2 s n)

broadcastTransaction :: Transaction -> [(HostName, ServiceName)] -> IO ()
broadcastTransaction t = mapM_ sendMsg
  where
    msg = encodeStrict t
    sendMsg :: (HostName, ServiceName) -> IO ()
    sendMsg (host, port) = connect host port $ \(sock, _) -> do send sock msg

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
    validateSender acc = availableBalance acc >= txCost t
    senderAcc = Map.lookup (senderAddress t) m
