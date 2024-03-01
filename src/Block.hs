{-# LANGUAGE InstanceSigs #-}

module Block
  ( Block (..),
    Blockchain,
    createBlock,
    validateBlock,
    emptyBlock,
  )
where

import Codec.Crypto.RSA (PublicKey (..))
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Binary ( Binary(get, put), encode, Get, Put )
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.UnixTime (UnixTime (..))
import Transaction (Transaction)

data BlockInit = BlockInit
  { initIndex :: Int, -- index number of block
    initTimestamp :: UnixTime, -- microseconds since 1st Jan 1970
    initTransactions :: [Transaction], -- list of txs
    initValidator :: PublicKey, -- public key of the node that validated the tx
    initPreviousHash :: ByteString -- the hash of the previous block
  }

type Blockchain = [Block]

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

validateBlock :: Block -> Block -> PublicKey -> Bool
validateBlock newblock prevblock validator = validatorOK && hashOK
  where
    validatorOK = blockValidator newblock == validator
    hashOK = blockPreviousHash newblock == blockCurrentHash prevblock

-- validateChain :: Blockchain -> Bool
-- validateChain chain = foldr validateChain' True $ zip chain (tail chain)
--     where validateChain' :: (Block, Block) -> Bool -> Bool
--           validateChain' (new, prev) acc = acc && validateBlock new prev (blockValidator new)

-- I don't think this is ever going to be used, because for the time being the network is
-- static. A fixed number of nodes enter and that's it.
