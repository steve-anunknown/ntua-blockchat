module Block
  ( Block (..),
    createBlock,
    Blockchain,
    broadcastBlock
  )
where

import Utils
import Transaction      (Transaction)

import Codec.Crypto.RSA (PublicKey)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Binary
import Data.ByteArray   (convert)
import Data.ByteString  (ByteString)
import Data.UnixTime    (UnixTime)
import Network.Simple.TCP   
import qualified Data.ByteString.Lazy as B

data BlockInit = BlockInit
  { initIndex :: Int, -- index number of block
    initTimestamp :: UnixTime, -- microseconds since 1st Jan 1970
    initTransactions :: [Transaction], -- list of txs
    initValidator :: PublicKey, -- public key of the node that validated the tx
    initPreviousHash :: ByteString -- the hash of the previous block
  }

type Blockchain = [Block]

instance Binary BlockInit where
  put (BlockInit index time trans val prev) = do
    put index
    put time
    put trans
    put val
    put prev
  get = do
    index <- get
    time  <- get
    trans <- get
    val   <- get
    BlockInit index time trans val <$> get

data Block = Block
  { blockIndex :: Int, -- index number of block
    blockTimestamp :: UnixTime, -- microseconds since 1st Jan 1970
    blockTransactions :: [Transaction], -- list of txs
    blockValidator :: PublicKey, -- public key of the node that validated the tx
    blockPreviousHash :: ByteString, -- the hash of the previous block
    blockCurrentHash :: ByteString
  } deriving (Show, Eq)

instance Binary Block where
  put (Block index time trans val prev curr) = do
    put index
    put time
    put trans
    put val
    put prev 
    put curr
  get = do
    index <- get
    time  <- get
    trans <- get
    val   <- get
    prev  <- get
    Block index time trans val prev <$> get

computeBlockHash :: BlockInit -> ByteString
computeBlockHash = convert . hashWith SHA256 . B.toStrict . encode

finalizeBlock :: BlockInit -> Block
finalizeBlock initBlock =
  Block
    { blockIndex        = initIndex initBlock,
      blockTimestamp    = initTimestamp initBlock,
      blockTransactions = initTransactions initBlock,
      blockValidator    = initValidator initBlock,
      blockPreviousHash = initPreviousHash initBlock,
      blockCurrentHash  = computeBlockHash initBlock
    }

-- the capacity of transactions that the block holds is specified
-- by an environmental constant called "capacity".
createBlock :: Int -> UnixTime -> [Transaction] -> PublicKey -> ByteString -> Block
createBlock ind time list pub prev = finalizeBlock $ BlockInit ind time list pub prev

-- TODO: implement "mintBlock"

broadcastBlock :: Block -> [(HostName, ServiceName)] -> IO ()
broadcastBlock block = mapM_ sendBlock
    where msg = encodeStrict block
          sendBlock :: (HostName, ServiceName) -> IO ()
          sendBlock (host, port) = connect host port $ \(sock, _) -> do send sock msg
    

-- TODO: implement "validateBlock"
