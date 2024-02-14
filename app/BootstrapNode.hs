{-# LANGUAGE OverloadedStrings #-}

module BootstrapNode (
    BootInfo(..),
    BootState(..),
    BootApp,
    bootstrapNode,
)   where

import Block
import Wallet
import Transaction

import Control.Monad.State (StateT)
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
  } deriving (Show)

-- BootInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant. The IP and the PORT
-- of the Boot node are known to the other nodes beforehand.

data BootState = BootState
  { bootCurrID      :: Int
  , bootFriends     :: Map.Map PublicKey (HostName, ServiceName)
  , blockchain      :: Blockchain
  }

-- BootState is the state of the node regarding the blockchain.
-- It is mutable and changes as nodes connect to the network.
type BootApp = ReaderT BootInfo (StateT BootState IO)
type KeyNodeMap = Map.Map PublicKey (HostName, ServiceName)

-- reminder that StateT :: (s -> m (a, s)) -> StateT s m a
-- in this case:
-- (StateT BootState IO) :: (BootState -> IO (a, BootState))
-- that is, the return type of the computation is free.
--
-- ReaderT :: (r -> m a) -> ReaderT r m a
-- in this case:
-- (ReaderT BootInfo (StateT BootState IO)) ::
--                        (BootInfo -> StateT BootState IO a)
-- that is, the return type of the computation is free.
--
-- Therefore, BootApp also has a free return type.

bootstrapNode :: BootInfo -> IO (KeyNodeMap, Blockchain)
bootstrapNode = runReaderT bootstrapNodeLogic

bootstrapNodeLogic :: ReaderT BootInfo IO (KeyNodeMap, Blockchain)
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
        zeropub   = PublicKey 0 0 0
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
    
    liftIO $ putStrLn $ "Starting bootstrap node at " ++ myip ++ ":" ++ myport
    istate <- liftIO $ newIORef $ BootState 1 Map.empty []
    trigger <- liftIO newEmptyMVar
    _ <- liftIO $ forkIO $ serve myhost myport $ serverLogic trigger istate -- thread that never returns
    _ <- liftIO $ takeMVar trigger -- main thread blocks here until trigger is set
    fstate <- liftIO $ readIORef istate
    let friendMap = bootFriends fstate :: KeyNodeMap
        keys    = Map.keys friendMap :: [PublicKey]
        friends = Map.elems friendMap :: [(HostName, ServiceName)]
    _ <- liftIO $ mapM (\(ip, port) -> connect ip port $ \x -> send (fst x) $ encodeStrict (keys, friends, genesisBl)) friends

    liftIO $ putStrLn $ "Map is " ++ show friendMap
    liftIO $ putStrLn $ "Genesis is " ++ show genesisBl
    return (friendMap, [genesisBl])


encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: Binary a => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)
