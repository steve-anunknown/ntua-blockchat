module Block (
    Block (..),
    createBlock
) where

import Codec.Crypto.RSA (PublicKey)
import Transaction (Transaction)
import Data.UnixTime (UnixTime)
import Data.ByteString.Lazy.Internal as LBSI

data Block = Block {
    blockIndex :: Int, -- index number of block
    blockTimestamp :: UnixTime, -- microseconds since 1st Jan 1970
    blockTransactions :: [Transaction], -- list of transactions
    blockValidator :: PublicKey, -- public key of the node that validated the transaction,
    blockPreviousHash :: LBSI.ByteString, -- the hash of the previous block
    blockCurrentHash :: Maybe LBSI.ByteString -- the hash of the block
                                     -- it's a Maybe because at some point it may
                                     -- not be hashed yet.
}
-- the capacity of transactions that the block holds is specified
-- by an environmental constant called "capacity".
createBlock = Block


-- TODO: implement "mintBlock"


-- TODO: implement "broadcastBlock"


-- TODO: implement "validateBlock"
