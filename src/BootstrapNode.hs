module BootstrapNode (
    BootInfo(..),
    BootState(..),
    bootstrapNode,
)   where

import Utils
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

data BootInfo = BootInfo
  { bootNodeID      :: Int
  , bootNodeIP      :: HostName
  , bootNodePort    :: ServiceName
  , bootNodeNumb    :: Int -- number of nodes to insert into the network 
  } deriving (Show, Eq)

-- BootInfo does not change. It is set once, using the arguments
-- passed to the program and then remains constant. The IP and the PORT
-- of the Boot node are known to the other nodes beforehand.

-- data BootState = BootState
--   { bootCurrID      :: Int
--   , bootFriends     :: KeyNodeMap
--   , bootBlockchain  :: Blockchain
--   } deriving (Show, Eq)

-- TODO: Redesign the Boot State.
-- The Boot State has to be redesigned. It should be:
data BootState = BootState {
  bootCurrID :: Int,
  bootPublicKeys :: [PublicKey],
  bootPeers :: [(HostName, ServiceName)],
  bootBlockchain :: Blockchain
 } deriving (Show, Eq)

bootstrapNode :: BootInfo -> IO BootState
bootstrapNode = runReaderT bootstrapNodeLogic

bootstrapNodeLogic :: ReaderT BootInfo IO BootState
bootstrapNodeLogic = do
    info <- ask -- get the environment
    time <- liftIO getUnixTime -- get the current time
    (mypub, mypriv) <- liftIO $ generateWallet 2048-- get a wallet
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
        updateState (pub, ip, port) s = (s {bootPublicKeys = newKeys, bootPeers = newPeers}, ())
            where newKeys = pub : bootPublicKeys s
                  newPeers = (ip, port) : bootPeers s
        incrementID :: BootState -> (BootState, Int)
        incrementID s = (s {bootCurrID = newID}, newID)
            where newID = bootCurrID s + 1
        -- write the server logic. Assign an ID to each node, build up
        -- a map from public keys to (ip, port) pairs, and broadcast
        -- the map and the genesis blocks.
        serverLogic :: [MVar Int] -> IORef BootState -> (Socket, SockAddr) -> IO ()
        serverLogic trigger ioref (socket, _) = do
            currID <- atomicModifyIORef' ioref incrementID
            when (currID <= totalNodes) $ do
                msg <- recv socket 512 -- 512 bytes should be enough to hold a public key (2048), an ip and a port
                let keyval = decodeMaybe msg :: (PublicKey, HostName, ServiceName)
                atomicModifyIORef' ioref $ updateState keyval
                send socket $ encodeStrict currID
                putMVar (trigger !! (currID - 1)) 1
    -- setup the state and locks
    state <- liftIO $ newIORef $ BootState 0 [][][]
    trigger <- liftIO $ mapM (const newEmptyMVar) [1..totalNodes]
    -- start serving
    _ <- liftIO $ forkIO $ serve myhost myport $ serverLogic trigger state -- thread that never returns
    _ <- liftIO $ mapM takeMVar trigger -- wait for all nodes to connect
    _ <- liftIO $ modifyIORef' state $ \s -> (s {bootBlockchain = [genesisBl]})
    -- get the final state
    fstate <- liftIO $ readIORef state
    let keys    = bootPublicKeys fstate :: [PublicKey]
        friends = bootPeers fstate :: [(HostName, ServiceName)]
        msg     = encodeStrict (keys, friends, genesisBl)
    _ <- liftIO $ mapM (\(ip, port) -> connect ip port $ \x -> send (fst x) msg) friends
    liftIO $ readIORef state

