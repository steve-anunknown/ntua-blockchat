{-# LANGUAGE OverloadedStrings #-}

module BootstrapNode (
    BootInfo(..),
    BootState(..),
    bootstrapNode,
)   where

import Block
import Wallet
import Transaction

import Control.Monad.Reader
import Control.Monad (when)
import Control.Concurrent
import Data.IORef
import Data.UnixTime
import Network.Simple.TCP
import Codec.Crypto.RSA (PublicKey(..))
import Data.Binary (Binary, encode, decode)


import qualified Data.ByteString as BS
import qualified Data.Map as Map

data BootInfo = BootInfo
  { bootNodeID      :: Int
  , bootNodeIP      :: HostName
  , bootNodePort    :: ServiceName
  , bootNodeNumb    :: Int -- number of nodes to insert into the network 
  } deriving (Show, Eq)

-- BootInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant. The IP and the PORT
-- of the Boot node are known to the other nodes beforehand.

type KeyNodeMap = Map.Map PublicKey (HostName, ServiceName)

data BootState = BootState
  { bootCurrID      :: Int
  , bootFriends     :: KeyNodeMap
  , bootBlockchain      :: Blockchain
  } deriving (Show, Eq)

bootstrapNode :: BootInfo -> IO BootState
bootstrapNode = runReaderT bootstrapNodeLogic

bootstrapNodeLogic :: ReaderT BootInfo IO BootState
bootstrapNodeLogic = do
    info <- ask -- get the environment
    time <- liftIO getUnixTime -- get the current time
    (mypub, mypriv) <- liftIO $ generateWallet 512 -- get a wallet
    let -- extract info from the environment
        myip       = bootNodeIP info
        myport     = bootNodePort info
        totalNodes = bootNodeNumb info
        myhost     = Host myip
        -- setup the genesis block
        prevHash  = encodeStrict (1 :: Int)
        zeropub   = PublicKey 0 0 65537
        txtype    = Coins $ 1000 * fromIntegral totalNodes
        genesisTx = createTransaction zeropub mypub txtype 1 mypriv
        genesisBl = createBlock 1 time [genesisTx] zeropub prevHash
        -- helper to update state while running the server
        updateState ::  (PublicKey, HostName, ServiceName) -> BootState -> (BootState, ())
        updateState (pub, ip, port) s = (s {bootCurrID = bootCurrID s + 1, bootFriends = Map.insert pub (ip, port) (bootFriends s)}, ())
        -- write the server logic. Assign an ID to each node, build up
        -- a map from public keys to (ip, port) pairs, and broadcast
        -- the map and the genesis blocks.
        serverLogic :: MVar Int -> IORef BootState -> (Socket, SockAddr) -> IO ()
        serverLogic trigger ioref (socket, _) = do
            state <- readIORef ioref
            if bootCurrID state <= totalNodes then do
                msg <- recv socket 4096
                send socket $ encodeStrict (bootCurrID state)
                let keyval = decodeMaybe msg :: (PublicKey, HostName, ServiceName)
                atomicModifyIORef ioref $ updateState keyval
                state' <- readIORef ioref -- state has to be read again, don't be fooled
                when (bootCurrID state' == (totalNodes + 1)) $ do putMVar trigger 1
                closeSock socket
            else do 
                closeSock socket
    
    istate <- liftIO $ newIORef $ BootState 1 Map.empty []
    trigger <- liftIO newEmptyMVar
    _ <- liftIO $ forkIO $ serve myhost myport $ serverLogic trigger istate -- thread that never returns
    _ <- liftIO $ takeMVar trigger -- main thread blocks here until trigger is set
    fstate <- liftIO $ readIORef istate
    let friendMap = bootFriends fstate :: KeyNodeMap
        keys    = Map.keys friendMap :: [PublicKey]
        friends = Map.elems friendMap :: [(HostName, ServiceName)]
    -- broadcast
    _ <- liftIO $ mapM (\(ip, port) -> connect ip port $ \x -> send (fst x) $ encodeStrict (keys, friends, genesisBl)) friends
    
    _ <- liftIO $ atomicModifyIORef istate $ \s -> (s {bootBlockchain = [genesisBl]}, ())
    liftIO $ readIORef istate

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: Binary a => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)
