{-# LANGUAGE OverloadedStrings #-}

module OrdinaryNode ( 
    NodeInfo(..),
    NodeState(..),
    KeyNodeMap,
    ordinaryNode)   where

import Block
import Wallet

import Network.Simple.TCP
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.Reader
import Control.Concurrent 
import Data.IORef
import Data.Binary (Binary, encode, decode)
import qualified Data.ByteString as BS
import qualified Data.Map as Map

data NodeInfo = NodeInfo 
  { nodeIP      :: HostName
  , nodePort    :: ServiceName
  } deriving (Show, Eq)

-- NodeInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant.

type KeyNodeMap = Map.Map PublicKey (HostName, ServiceName)

data NodeState = NodeState 
  { nodeID          :: Int
  , nodeFriends     :: KeyNodeMap
  , nodeBlockchain  :: Blockchain
  } deriving (Show, Eq)

-- NodeState is the state of the node regarding the blockchain.
-- It is mutable and changes as nodes connect to the network.

ordinaryNode :: HostName -> ServiceName -> NodeInfo -> IO NodeState
ordinaryNode bip bport = runReaderT $ ordinaryNodeLogic bip bport

ordinaryNodeLogic :: HostName -> ServiceName -> ReaderT NodeInfo IO NodeState
ordinaryNodeLogic bip bport = do
    info <- ask
    (pub, _) <- liftIO $ generateWallet 512
    let ip   = nodeIP info :: HostName
        port = nodePort info :: ServiceName
        msg  = encodeStrict (pub, ip, port)
        
        getIDfromBoot :: IORef NodeState -> (Socket, SockAddr) -> IO()
        getIDfromBoot ioref (socket, _) = do
            send socket msg
            resp <- recv socket 32
            atomicModifyIORef ioref $ \s -> (s {nodeID = decodeMaybe resp}, ())
            closeSock socket

        getBroadcast :: MVar Int -> IORef NodeState -> (Socket, SockAddr) -> IO()
        getBroadcast mvar ioref (socket, _) = do
            resp <- recv socket 4096
            let (keys, friends, genesis) = decodeMaybe resp :: ([PublicKey], [(HostName, ServiceName)], Block)
                friendsMap = Map.fromList $ zip keys friends
            atomicModifyIORef' ioref $ \s -> (s {nodeFriends = friendsMap, nodeBlockchain = [genesis]}, ())
            putMVar mvar 1
            closeSock socket

    state <- liftIO $ newIORef (NodeState 0 Map.empty [])
    _ <- liftIO $ connect bip bport $ getIDfromBoot state 

    trigger <- liftIO newEmptyMVar
    _ <- liftIO $ forkIO $ serve (Host ip) port $ getBroadcast trigger state 
    _ <- liftIO $ takeMVar trigger

    liftIO $ readIORef state -- return the final state

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: Binary a => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)
