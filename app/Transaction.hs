module Transaction (
    TransactionType (..),
    Transaction (..),
    TransactionSigned (..),
    transactionCost,
    createTransaction,
    signTransaction, broadcastTransaction,
    verifySignature,
    validateTransaction
) where

import qualified Data.Map as Map
import Data.Serialize
import Crypto.Hash
import Account          (Account(..), availableBalance)
import qualified Data.ByteString.Lazy as LBSI
import Codec.Crypto.RSA (PublicKey(..), PrivateKey, sign, verify)

---------------------------------------------------------
-- PublicKey is not an instance of Ord, so we make it one
-- in order to be able to index Maps using it.
instance Ord PublicKey where
    (<=) PublicKey{public_n = pn1} PublicKey{public_n = pn2} = pn1 <= pn2
-- This is perhaps bad design, I don't know.
---------------------------------------------------------

data TransactionType = Coins Double
                     | Message String
                     | Both (Double, String) deriving(Show)

data Transaction = Transaction {
    transSenderAddress :: PublicKey, -- public key of the wallet which the transaction comes from
    transReceiverAddress :: PublicKey, -- public key of the wallet which the transaction sends to
    transType :: TransactionType, -- type of the transaction
    transNonce :: Int, -- counter 
    transId :: LBSI.ByteString -- the hash of the transaction
}

-- a Maybe Bytestring used to be used for the signature of the transaction
-- but i changed my mind and believe that this is better.
-- The downside is that, in order to extract transaction info from 
-- the new type, a double unwrapping is needed. E.g transSenderAddress (transaction signedTransaction)
data TransactionSigned = TransactionSigned {
    transaction :: Transaction,
    signature :: LBSI.ByteString
}

transactionCost :: TransactionType -> Double
transactionCost (Coins amount) = amount + amount * 3 / 100 -- 3 % fee
transactionCost (Message mess) = amount + amount * 3 / 100 -- TODO: not sure exactly when the fee applies
    where amount = fromIntegral $ Prelude.length mess
transactionCost (Both (amount,mess)) = total + total * 3 / 100
    where total = amount + (fromIntegral $ Prelude.length mess)


-- TODO: Figure out how to hash the transaction fields
-- Serialize a Transaction into a Lazy ByteString
serializeTransaction :: Transaction -> LBSI.ByteString
serializeTransaction = encodeLazy

-- Hash a Transaction
hashTransaction :: Transaction -> LBSI.ByteString
hashTransaction = hashLazy . serializeTransaction


-- this is a pure function and just creates a transaction.
-- it is just a wrapper around the constructor
--createTransaction :: PublicKey -> PublicKey -> TransactionType -> Int -> LBSI.ByteString -> Transaction
--createTransaction = Transaction

-- Create a new Transaction with the transId field filled in
createTransaction :: PublicKey -> PublicKey -> TransactionType -> Int -> Transaction
createTransaction sender receiver tType nonce =
    let
        transactionData = Transaction sender receiver tType nonce LBSI.empty -- Leave transId empty for now
        transactionHash = hashTransaction transactionData
    in
        transactionData { transId = transactionHash } -- Fill in the transId field with the hash

signTransaction :: PrivateKey -> Transaction -> TransactionSigned
signTransaction privKey trans =
    let
        transHash = transId trans
        signHash = sign privKey transHash
    in
        TransactionSigned trans signHash

-- TODO: implement "broadcastTransaction" 
broadcastTransaction :: a -> a
broadcastTransaction _ = error "Not implemented"

verifySignature :: TransactionSigned -> Bool
verifySignature t =
    let
        transInfo = transaction t
        signHash = signature t

        fromAddress = transSenderAddress transInfo
        messageHash = transId transInfo
    in
        verify fromAddress messageHash signHash

-- PublicKey has been made an instance of Ord, therefore it can be used
-- as a key in a map.
validateTransaction :: TransactionSigned -> Map.Map PublicKey Account -> Bool
validateTransaction t m = maybe False validateSender senderAcc
    where -- maybe has type b -> (a -> b) -> Maybe a -> b 
        sigOk = verifySignature t :: Bool
        transInfo = transaction t :: Transaction
        senderAcc = Map.lookup (transSenderAddress transInfo) m :: Maybe Account 
        validateSender :: Account -> Bool
        validateSender acc = sigOk && balanceOk
            where
                transCost = transactionCost $ transType transInfo
                balanceOk = availableBalance acc >= transCost


