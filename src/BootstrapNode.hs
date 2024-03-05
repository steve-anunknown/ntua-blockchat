{-# LANGUAGE OverloadedStrings #-}

module BootstrapNode
  ( BootInfo (..),
    BootState (..),
    BootstrapNode (..),
    bootstrapNode,
  )
where

import Block (Block, Blockchain, createBlock)
import Codec.Crypto.RSA (PublicKey (..))
import Control.Concurrent
  ( MVar,
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Monad (when)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    ReaderT (runReaderT),
    asks,
  )
import qualified Data.ByteString as BS
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.UnixTime (UnixTime, getUnixTime)
import Network.Simple.TCP
  ( HostName,
    HostPreference (Host),
    ServiceName,
    SockAddr,
    Socket,
    connect,
    recv,
    send,
    serve,
  )
import ServiceType (ServiceType (Coins))
import Transaction (Transaction, createTransaction, zeropub)
import Utils (decodeMaybe, encodeStrict)
import Wallet (Wallet, generateWallet)

data BootstrapNode = BootstrapNode HostName ServiceName

data BootInfo = BootInfo
  { bootNodeID :: Int,
    bootNodeIP :: HostName,
    bootNodePort :: ServiceName,
    bootNodeNumb :: Int -- number of nodes to insert into the network
  }
  deriving (Show, Eq)

-- BootInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant. The IP and the PORT
-- of the Boot node are known to the other nodes beforehand.

data BootState = BootState
  { bootCurrID :: Int,
    bootPublicKeys :: [(Int, PublicKey)],
    bootPeers :: [(HostName, ServiceName)],
    bootBlockchain :: Blockchain
  }
  deriving (Show, Eq)

emptyBootState :: BootState
emptyBootState = BootState 0 [] [] []

bootstrapNode :: BootInfo -> IO BootState
bootstrapNode = runReaderT bootstrapNodeLogic

-- | This is a helper function that updates the state of the boot node
updateState :: (Int, (PublicKey, HostName, ServiceName)) -> BootState -> (BootState, ())
updateState t s = (s {bootPublicKeys = newKeys, bootPeers = newPeers}, ())
  where
    (num, (pub, ip, port)) = t
    newKeys = (num, pub) : bootPublicKeys s
    newPeers = (ip, port) : bootPeers s

-- | This is a helper function that increments the current ID of the boot state
incrementID :: BootState -> (BootState, Int)
incrementID s = (s {bootCurrID = newID}, newID)
  where
    newID = bootCurrID s + 1

-- | This is the function that handles the server logic
server :: [MVar Int] -> IORef BootState -> (Socket, SockAddr) -> IO ()
server triggers ioref (socket, _) = do
  currID <- atomicModifyIORef' ioref incrementID
  when (currID <= length triggers) $ do
    msg <- recv socket 4096 -- should be enough to hold a public key (2048), an ip and a port
    let keyval = decodeMaybe msg :: (PublicKey, HostName, ServiceName)
    atomicModifyIORef' ioref $ updateState (currID, keyval)
    send socket $ encodeStrict currID
    putMVar (triggers !! (currID - 1)) 1

createGenesisTX :: Wallet -> Int -> Transaction
createGenesisTX (pub, priv) totalnodes = createTransaction zeropub pub tx 1 priv
  where
    tx = Coins $ 1000 * fromIntegral totalnodes

createGenesisBlock :: UnixTime -> Wallet -> Int -> Block
createGenesisBlock time wallet totalnodes = createBlock 1 time [genesisTX] zeropub prevHash
  where
    prevHash = encodeStrict (1 :: Int)
    genesisTX = createGenesisTX wallet totalnodes

bootstrapNodeLogic :: ReaderT BootInfo IO BootState
bootstrapNodeLogic = do
  myip <- asks bootNodeIP
  myport <- asks bootNodePort
  mywallet <- liftIO $ generateWallet 2048 -- get a wallet
  totalNodes <- asks bootNodeNumb
  -- setup the state and locks
  state <- liftIO $ newIORef emptyBootState
  triggers <- liftIO $ mapM (const newEmptyMVar) [1 .. totalNodes]
  _ <- liftIO $ forkIO $ serve (Host myip) myport $ server triggers state -- thread that never returns
  liftIO $ mapM_ takeMVar triggers -- wait for all nodes to connect
  time <- liftIO getUnixTime
  fstate <- liftIO $ readIORef state
  let genesisBl = createGenesisBlock time mywallet totalNodes
      final = fstate {bootBlockchain = [genesisBl]}
      keys = bootPublicKeys final :: [(Int, PublicKey)]
      friends = bootPeers final :: [(HostName, ServiceName)]
      msg = BS.append "0" (encodeStrict (keys, friends, genesisBl))

  -- reversing the list of friends seems to actually matter, at least when creating nodes
  -- from the command line. It seems that the bootstrap node tries to connect to the last
  -- node to enter the network too fast, before the node has time to start the server.
  liftIO $ mapM_ (\(ip, port) -> connect ip port (\x -> send (fst x) msg)) (reverse friends)
  return final
