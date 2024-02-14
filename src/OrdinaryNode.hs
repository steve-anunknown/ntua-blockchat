{-# LANGUAGE OverloadedStrings #-}

module OrdinaryNode ( 
    NodeInfo(..),
    NodeState(..),
    NodeApp,
    KeyNodeMap,
    ordinaryNode)   where

import Block
import Wallet

import Network.Simple.TCP
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.State (StateT)
import Control.Monad.Reader
import Control.Concurrent 
import Data.IORef
import Data.Binary (Binary, encode, decode)
import qualified Data.ByteString as BS
import qualified Data.Map as Map

data NodeInfo = NodeInfo 
  { nodeIP      :: HostName
  , nodePort    :: ServiceName
  } deriving (Show)

-- NodeInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant.

type KeyNodeMap = Map.Map PublicKey (HostName, ServiceName)

data NodeState = NodeState 
  { nodeID      :: Int
  , nodeFriends     :: KeyNodeMap
  , nodeblockchain  :: Blockchain
  }

-- NodeState is the state of the node regarding the blockchain.
-- It is mutable and changes as nodes connect to the network.

type NodeApp = ReaderT NodeInfo (StateT NodeState IO)
-- reminder that StateT :: (s -> m (a, s)) -> StateT s m a
-- in this case, (StateT NodeState IO) :: (NodeState -> IO (a, NodeState))
-- that is, the return type of the computation is free.
--
-- ReaderT :: (r -> m a) -> ReaderT r m a
-- in this case, (ReaderT NodeInfo (StateT NodeState IO)) :: (NodeInfo -> StateT NodeState IO a)
-- that is, the return type of the computation is free.
--
-- Therefore, NodeApp also has a free return type.


ordinaryNode :: HostName -> ServiceName -> NodeInfo -> IO (KeyNodeMap, Blockchain)
ordinaryNode bip bport = runReaderT $ ordinaryNodeLogic bip bport

ordinaryNodeLogic :: HostName -> ServiceName -> ReaderT NodeInfo IO (KeyNodeMap, Blockchain)
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
            atomicModifyIORef' ioref $ \s -> (s {nodeFriends = friendsMap, nodeblockchain = [genesis]}, ())
            putMVar mvar 1
            closeSock socket
    
    --liftIO $ putStrLn $ "Starting ordinary node at " ++ ip ++ ":" ++ port

    state <- liftIO $ newIORef (NodeState 0 Map.empty [])
    _ <- liftIO $ connect bip bport $ getIDfromBoot state 
    -- readState <- liftIO $ readIORef state
    -- liftIO $ putStrLn $ "Node ID: " ++ show (nodeID readState)

    trigger <- liftIO newEmptyMVar
    _ <- liftIO $ forkIO $ serve (Host ip) port $ getBroadcast trigger state 
    _ <- liftIO $ takeMVar trigger

    final <- liftIO $ readIORef state -- important to read the state again.
    -- liftIO $ print (nodeFriends final)
    -- liftIO $ print (nodeblockchain final)

    return (nodeFriends final, nodeblockchain final)


encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: Binary a => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)
