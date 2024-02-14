{-# LANGUAGE TupleSections #-}
module OrdinaryNode ( 
    NodeInfo(..),
    NodeState(..),
    ordinaryNode)   where

import Block
import Utils
import Wallet
import Account

import Network.Simple.TCP
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.Reader
import Control.Concurrent 
import Data.IORef
import qualified Data.Map as Map

data NodeInfo = NodeInfo 
  { nodeIP      :: HostName
  , nodePort    :: ServiceName
  } deriving (Show, Eq)

-- NodeInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant.

-- data NodeState = NodeState 
--   { nodeID          :: Int
--   , nodeFriends     :: KeyNodeMap
--   , nodeBlockchain  :: Blockchain
--   } deriving (Show, Eq)

-- TODO: Redesign the Node State.
-- The Node State shouldn't be like that. It really should be:
data NodeState = NodeState{
   nodeID :: Int,
   nodeAccounts :: Map.Map PublicKey Account,
   nodePeers :: [NodeInfo],
   nodeBlockchain :: Blockchain
} deriving (Show, Eq)

-- NodeState is the state of the node regarding the blockchain.
-- It is mutable and changes as nodes connect to the network.

ordinaryNode :: HostName -> ServiceName -> NodeInfo -> IO NodeState
ordinaryNode bip bport = runReaderT $ ordinaryNodeLogic bip bport

ordinaryNodeLogic :: HostName -> ServiceName -> ReaderT NodeInfo IO NodeState
ordinaryNodeLogic bip bport = do
    info     <- ask
    (pub, _) <- liftIO $ generateWallet 2048
    state    <- liftIO $ newIORef (NodeState 0 Map.empty [] [])
    trigger  <- liftIO newEmptyMVar

    let ip   = nodeIP info :: HostName
        port = nodePort info :: ServiceName
        msg  = encodeStrict (pub, ip, port)
        page = 4096
        
        getIDfromBoot :: IORef NodeState -> (Socket, SockAddr) -> IO()
        getIDfromBoot ioref (socket, _) = do
            send socket msg
            resp <- recv socket 32 -- 32 bytes enough to hold an Int
            modifyIORef' ioref $ \s -> s {nodeID = decodeMaybe resp}

        getBroadcast :: MVar Int -> IORef NodeState -> (Socket, SockAddr) -> IO()
        getBroadcast mvar ioref (socket, _) = do
            resp <- recv socket $ 8 * page -- 32KB ~ 100 nodes
            let (keys, friends, genesis) = decodeMaybe resp :: ([PublicKey], [(HostName, ServiceName)], Block)
                accountMap = Map.fromList $ map (, emptyAccount) keys
                peers = map (uncurry NodeInfo) friends
            modifyIORef' ioref $ \s -> s {nodeAccounts = accountMap, nodePeers = peers, nodeBlockchain = [genesis]}
            putMVar mvar 1
    -- connect to the boot to get your id
    _ <- liftIO $ connect bip bport $ getIDfromBoot state 
    -- setup server to receive broadcast
    _ <- liftIO $ forkIO $ serve (Host ip) port $ getBroadcast trigger state 
    _ <- liftIO $ takeMVar trigger -- wait for the broadcast to finish
    liftIO $ readIORef state -- return the final state


