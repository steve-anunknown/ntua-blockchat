module OrdinaryNode ( 
    NodeInfo(..),
    NodeState(..),
    NodeApp 
)   where

import Block
import Network.Simple.TCP
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

data NodeInfo = NodeInfo 
  { nodeID      :: Int
  , nodeIP      :: HostName
  , nodePort    :: ServiceName
  } deriving (Show)

-- NodeInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant.

data NodeState = NodeState 
  { nodeFriends     :: Map.Map PublicKey (HostName, ServiceName)
  , nodeblockchain  :: Blockchain
  }

-- NodeState is the state of the node regarding the blockchain.
-- It is mutable and changes as nodes connect to the network.

type NodeApp = ReaderT NodeInfo (StateT NodeState IO)

