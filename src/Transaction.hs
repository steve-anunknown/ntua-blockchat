{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
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
    updateAccsByTXs,
    zeropub,
  )
where

import Account (availableBalance, updateBalanceBy, updateStake)
import Codec.Crypto.RSA (PrivateKey, PublicKey (..), sign, verify)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Binary (Binary (get, put), encode)
import Data.ByteArray (convert)
import Data.ByteString (ByteString, append)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Network.Simple.TCP (HostName, ServiceName, connect, send)
import ServiceType (ServiceType (..), serviceCost, serviceFee)
import Types (Peer, PubKeyToAcc)
import Utils (encodeStrict)

---------------------------------------------------------
-- PublicKey is not an instance of Ord, so we make it one
-- in order to be able to index Maps using it.
instance Ord PublicKey where
  (<=) :: PublicKey -> PublicKey -> Bool
  (<=) PublicKey {public_n = pn1} PublicKey {public_n = pn2} = pn1 <= pn2

-- This is perhaps bad design, I don't know.
---------------------------------------------------------

zeropub :: PublicKey
zeropub = PublicKey 0 0 65537

txMsgHeader :: ByteString
txMsgHeader = "1"

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

-- | This function takes a transaction as an argument and returns the cost of the transaction.
-- This includes the coins and the fee, in the case where coins are send.
txCost :: Transaction -> Double
txCost = serviceCost . serviceType

-- | This function takes a list of transactions as an argument and returns the sum of the fees of the transactions.
-- It is to be used in the minting phase.
txsFee :: [Transaction] -> Double
txsFee = sum . map txFee

-- | This function takes a transaction as an argument and returns the fee of the transaction.
-- It does not include the coins, only the fee.
txFee :: Transaction -> Double
txFee Transaction {serviceType = service} = case service of
  Coins c -> c * serviceFee
  Message msg -> fromIntegral (length msg)
  Both (c, msg) -> c * serviceFee + fromIntegral (length msg)

-- | This function takes a transaction and a state of accounts as arguments and returns
-- a new state of accounts, as a result of the transaction. It does not update the nonce
updateAccsByTX :: Transaction -> PubKeyToAcc -> PubKeyToAcc
updateAccsByTX t m = case serviceType t of
  Coins c ->
    -- if the receiver is the zeropub, then the transaction is a staking transaction
    if receiverAddress t == zeropub
      then
        Map.adjust (updateBalanceBy (-txFee t) . updateStake c) (senderAddress t) m
      else
        let cost = txCost t
            sender = senderAddress t
            receiver = receiverAddress t
            temp = Map.adjust (updateBalanceBy (-cost)) sender m
         in Map.adjust (updateBalanceBy c) receiver temp
  Message _ ->
    let cost = txCost t
        sender = senderAddress t
     in Map.adjust (updateBalanceBy (-cost)) sender m
  Both (c, _) ->
    let cost = txCost t
        sender = senderAddress t
        receiver = receiverAddress t
        temp = Map.adjust (updateBalanceBy (-cost)) sender m
     in Map.adjust (updateBalanceBy c) receiver temp

-- | This function takes a list of transactions and a state of accounts as arguments and returns
-- a new state of accounts, as a result of the transactions.
updateAccsByTXs :: [Transaction] -> PubKeyToAcc -> PubKeyToAcc
updateAccsByTXs txs initial = foldr updateAccsByTX initial txs

-- | This function takes a transaction (only the initial fields) and returns its hash.
computeHashID :: TransactionInit -> ByteString
computeHashID = convert . hashWith SHA256 . B.toStrict . encode

-- | This function takes a private key and a bytestring and returns the signature of the bytestring.
computeSignature :: PrivateKey -> ByteString -> ByteString
computeSignature privkey bytestring = B.toStrict $ sign privkey (B.fromStrict bytestring)

-- | This function takes a transaction and a private key and
-- returns the transaction with the signature field filled.
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

-- | This function takes a public key (sender), another public key (receiver), a service type,
-- a counter and a private key and returns a transaction.
createTransaction :: PublicKey -> PublicKey -> ServiceType -> Int -> PrivateKey -> Transaction
createTransaction p1 p2 s n = finalizeTransaction (TransactionInit p1 p2 s n)

-- | Broadcasts a transaction to a list of peers.
broadcastTransaction :: [Peer] -> Transaction -> IO ()
broadcastTransaction peers t = mapM_ sendMsg peers
  where
    msg = append txMsgHeader $ encodeStrict t
    sendMsg :: (HostName, ServiceName) -> IO ()
    sendMsg (host, port) = connect host port $ \(sock, _) -> send sock msg

-- | Verifies the signature of a transaction.
verifySignature :: Transaction -> Bool
verifySignature t = verify from tid sig
  where
    -- pay attention to the order of the arguments
    from = senderAddress t
    tid = B.fromStrict $ hashID t
    sig = B.fromStrict $ signature t

-- | This function takes a transaction and a map of public keys to accounts and returns
-- whether the transaction is valid or not.
validateTransaction :: Transaction -> PubKeyToAcc -> Bool
validateTransaction t m = verifySignature t && maybe False validateSender senderAcc
  where
    validateSender acc = availableBalance acc >= txCost t
    senderAcc = Map.lookup (senderAddress t) m
