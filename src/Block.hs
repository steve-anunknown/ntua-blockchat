{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Block
  ( Block (..),
    Blockchain,
    createBlock,
    validateBlock,
    emptyBlock,
    broadcastBlock,
    txIsUnique,
    meanBlockTime,
  )
where

import Codec.Crypto.RSA (PublicKey (..))
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Binary (Binary (get, put), Get, Put, encode)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.UnixTime (UnixTime (..), toClockTime)
import Network.Simple.TCP (HostName, ServiceName, connect, send)
import System.Time
import Transaction (Transaction)
import Types (Peer)
import Utils (encodeStrict)

data BlockInit = BlockInit
  { initIndex :: Int, -- index number of block
    initTimestamp :: UnixTime, -- microseconds since 1st Jan 1970
    initTransactions :: [Transaction], -- list of txs
    initValidator :: PublicKey, -- public key of the node that validated the tx
    initPreviousHash :: ByteString -- the hash of the previous block
  }

instance Binary BlockInit where
  put :: BlockInit -> Put
  put (BlockInit index time trans val prev) = do
    put index
    put time
    put trans
    put val
    put prev
  get :: Get BlockInit
  get = do
    index <- get
    time <- get
    trans <- get
    val <- get
    BlockInit index time trans val <$> get

data Block = Block
  { blockIndex :: Int, -- index number of block
    blockTimestamp :: UnixTime, -- microseconds since 1st Jan 1970
    blockTransactions :: [Transaction], -- list of txs
    blockValidator :: PublicKey, -- public key of the node that validated the tx
    blockPreviousHash :: ByteString, -- the hash of the previous block
    blockCurrentHash :: ByteString
  }
  deriving (Show, Eq)

instance Binary Block where
  put :: Block -> Put
  put (Block index time trans val prev curr) = do
    put index
    put time
    put trans
    put val
    put prev
    put curr
  get :: Get Block
  get = do
    index <- get
    time <- get
    trans <- get
    val <- get
    prev <- get
    Block index time trans val prev <$> get

computeBlockHash :: BlockInit -> ByteString
computeBlockHash = convert . hashWith SHA256 . B.toStrict . encode

finalizeBlock :: BlockInit -> Block
finalizeBlock initBlock =
  Block
    { blockIndex = initIndex initBlock,
      blockTimestamp = initTimestamp initBlock,
      blockTransactions = initTransactions initBlock,
      blockValidator = initValidator initBlock,
      blockPreviousHash = initPreviousHash initBlock,
      blockCurrentHash = computeBlockHash initBlock
    }

emptyBlock :: Block
emptyBlock = Block 0 (UnixTime 0 0) [] (PublicKey 0 0 65537) BS.empty BS.empty

-- the capacity of transactions that the block holds is specified
-- by an environmental constant called "capacity".
createBlock :: Int -> UnixTime -> [Transaction] -> PublicKey -> ByteString -> Block
createBlock ind time list pub prev = finalizeBlock $ BlockInit ind time list pub prev

type Blockchain = [Block]

blockMsgHeader :: ByteString
blockMsgHeader = "2"

validateBlock :: Block -> Block -> PublicKey -> Bool
validateBlock newblock prevblock validator = validatorOK && hashOK
  where
    validatorOK = blockValidator newblock == validator
    hashOK = blockPreviousHash newblock == blockCurrentHash prevblock

-- | Helper function to broadcast a block to all peers.
broadcastBlock :: [Peer] -> Block -> IO ()
broadcastBlock peers block = mapM_ sendBlock peers
  where
    msg = BS.append blockMsgHeader $ encodeStrict block
    sendBlock :: (HostName, ServiceName) -> IO ()
    sendBlock (host, port) = connect host port $ \(sock, _) -> send sock msg

-- | This function takes a transaction and a blockchain and returns True if the transaction is unique.
-- A transaction is unique if it is not present in any of the blocks in the blockchain.
txIsUnique :: Transaction -> Blockchain -> Bool
txIsUnique tx = all (notElem tx . blockTransactions)

-- | This function takes a ClockTime and returns the number of milliseconds since 1st Jan 1970.
milliseconds :: ClockTime -> Integer
milliseconds (TOD sec pico) = sec * 1000 + pico `div` 1000000000

-- | This function takes a blockchain and returns the mean time between blocks.
meanBlockTime :: Blockchain -> Double
meanBlockTime chain = fromIntegral (sum times'') / fromIntegral (length times)
  where
    times = map (Data.UnixTime.toClockTime . blockTimestamp) (reverse chain)
    times' = zip times (tail times)
    times'' = map (\(a, b) -> milliseconds b - milliseconds a) times'
