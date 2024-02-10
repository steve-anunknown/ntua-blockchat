module BootstrapNode (
    BootstrapInfo(..),
    BootstrapState(..),
    BootstrapApp
)   where

import Block
import Network.Simple.TCP
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

data BootstrapInfo = BootstrapInfo
  { bootNodeID      :: Int
  , bootNodeIP      :: HostName
  , bootNodePort    :: ServiceName
  , bootNodeNumb    :: Int -- number of nodes to insert into the network
  } deriving (Show)

-- BootstrapInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant. The IP and the PORT
-- of the bootstrap node are known to the other nodes beforehand.

data BootstrapState = BootstrapState
  { bootFriends     :: Map.Map PublicKey (HostName, ServiceName)
  , blockchain      :: Blockchain
  }

-- BootstrapState is the state of the node regarding the blockchain.
-- It is mutable and changes as nodes connect to the network.

type BootstrapApp = ReaderT BootstrapInfo (StateT BootstrapState IO)

