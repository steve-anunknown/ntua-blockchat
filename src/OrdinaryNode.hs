{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OrdinaryNode
  ( NodeInfo (..),
    node,
    sampleValidator -- export it just for testing
  )
where

import Account
  ( Account (accountBalance, accountStake),
    initialAccount,
  )
import Block (Block (..), createBlock, emptyBlock, validateBlock)
import BootstrapNode (BootstrapNode (..))
import Codec.Crypto.RSA (PublicKey (..))
import Control.Concurrent
  ( MVar,
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( STM,
    TQueue,
    TVar,
    atomically,
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVarIO,
    writeTQueue,
    writeTVar,
  )
import Control.Monad (when)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    ReaderT (runReaderT),
    asks,
  )
import Control.Monad.State (evalState)
import Data.Binary (decode)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.RVar (sampleStateRVar)
import Data.Random.Distribution.Categorical (weightedCategorical)
import Data.UnixTime (getUnixTime)
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
import System.Random (RandomGen, mkStdGen)
import Transaction
  ( Transaction,
    txsFee,
    updateAccsByTX,
    updateAccsByTXs,
    validateTransaction,
  )
import Types (Peer, PubKeyToAcc)
import Utils (decodeMaybe, encodeStrict)
import Wallet (Wallet)
import CLI

type TXQueue = TQueue Transaction

type BLQueue = TQueue Block

type StartupState = ([PublicKey], [Peer], Block)

-- NodeInfo does not change. It is set once, using the arguments passed to the program and then remains constant.
data NodeInfo = NodeInfo
  { nodeIP :: HostName,
    nodePort :: ServiceName,
    nodeInfoWallet :: Wallet
  }
  deriving (Show, Eq)

enqueueTQ :: TQueue a -> a -> STM ()
enqueueTQ = writeTQueue

dequeueTQ :: TQueue a -> STM a
dequeueTQ = readTQueue

data ServerEnv = ServerEnv (TVar StartupState) (MVar Int) TXQueue BLQueue

-- | This will be used by the nodeLogic function. It is a server that receives
-- requests and discriminates using their header.
server :: ServerEnv -> (Socket, SockAddr) -> IO ()
server (ServerEnv startupState trigger queuedTxs queuedBlocks) (socket, _) = do
  -- the server is a producer of transactions and blocks
  resp <- recv socket $ 8 * 4096 -- 32KB ~ 100 nodes
  let (msgtype, msg) = BS.splitAt 1 $ Data.Maybe.fromMaybe BS.empty resp
  case msgtype of
    "0" -> do
      -- receive broadcast from bootstrap
      atomically $ writeTVar startupState (decode $ BS.fromStrict msg)
      putMVar trigger (1 :: Int) -- set the trigger that will allow the node to start
    "1" -> atomically $ enqueueTQ queuedTxs (decode $ BS.fromStrict msg) -- receive transaction
    "2" -> atomically $ enqueueTQ queuedBlocks (decode $ BS.fromStrict msg) -- receive block
    _ -> return () -- do nothing if header is not recognized

-- | Helper function to broadcast a block to all peers.
broadcastBlockTo :: [Peer] -> Block -> IO ()
broadcastBlockTo peers block = do
  -- spawn threads that send the block to each peer
  -- and block until all of them have received it
  triggers <- liftIO $ mapM (const newEmptyMVar) peers
  mapM_ (forkIO . sendBlock) (zip peers triggers)
  mapM_ takeMVar triggers
  where
    msg = BS.append (encodeStrict ("2" :: String)) (encodeStrict block)
    sendBlock :: ((HostName, ServiceName), MVar Int) -> IO ()
    sendBlock ((h, p), trig) = do
      connect h p $ \(sock, _) -> send sock msg
      putMVar trig (1 :: Int)

-- | This function simulate the lottery for the validator.
sampleValidator :: (RandomGen g) => g -> [(Double, Int)] -> Int
sampleValidator g probs = evalState (sampleStateRVar (weightedCategorical probs)) g

-- | Helper function to get a valid block from a queue of blocks
--  given the last block and the public key of the validator.
getValidatorBlockFrom :: BLQueue -> Block -> PublicKey -> IO Block
getValidatorBlockFrom qBlocks lastBlock valKey = do
  block <- atomically $ dequeueTQ qBlocks
  if validateBlock block lastBlock valKey
    then return block
    else getValidatorBlockFrom qBlocks lastBlock valKey

node :: BootstrapNode -> Int -> NodeInfo -> IO ()
node bootstrap capacity = runReaderT $ nodeLogic bootstrap capacity

nodeGetID :: BootstrapNode -> ReaderT NodeInfo IO Int
nodeGetID (BootstrapNode bip bport) = do
  ip <- asks nodeIP
  port <- asks nodePort
  (pub, _) <- asks nodeInfoWallet
  let msg = encodeStrict (pub, ip, port)
  myid <- liftIO $ newIORef 0
  let getIDfromBoot :: (Socket, SockAddr) -> IO ()
      getIDfromBoot (socket, _) = do
        send socket msg
        resp <- recv socket 32 -- 32 bytes enough to hold an Int
        writeIORef myid $ decodeMaybe resp
  _ <- liftIO $ connect bip bport getIDfromBoot
  liftIO $ readIORef myid

nodeLogic :: BootstrapNode -> Int -> ReaderT NodeInfo IO ()
nodeLogic bootstrap capacity = do
  -- start by setting up the startup state.
  ip <- asks nodeIP
  port <- asks nodePort
  myid <- nodeGetID bootstrap -- connect to bootstrap and get id
  mywallet@(mypub, _) <- asks nodeInfoWallet -- used in processTXs
  trigger <- liftIO newEmptyMVar -- initialize trigger for broadcast
  queuedTXs <- liftIO (newTQueueIO :: IO TXQueue)
  queuedBlocks <- liftIO (newTQueueIO :: IO BLQueue)
  startupState <- liftIO $ newTVarIO ([] :: [PublicKey], [] :: [Peer], emptyBlock)

  -- setup a server that never returns. it discriminates between
  -- the types of messages it receives and acts accordingly.
  _ <- liftIO . forkIO $ serve (Host ip) port $ server (ServerEnv startupState trigger queuedTXs queuedBlocks)
  _ <- liftIO $ takeMVar trigger -- wait for the broadcast to finish
  (keys, friends, genesis) <- liftIO $ readTVarIO startupState

  -- Up to this point, the broadcast has finished. Each node has its own id,
  -- a list of peers and the genesis block. Now we can start the node.

  -- create references for the shared state
  blockchainRef <- liftIO $ newIORef [genesis]
  accountRef <- liftIO $ newIORef initialAccount
  let 
      initialAccounts = Map.fromList $ map (,initialAccount) keys
      clishared = (blockchainRef, accountRef) :: CLISharedState
      peers = filter (/= (myid, mypub)) (zip [1 ..] keys)
      cliinfo = CLIInfo mywallet (Map.fromList peers) ip port
      -- This function processes transactions (keeping track of the counter) and mints
      -- a new block when the counter reaches the capacity.
      processTXs :: IO ()
      processTXs = processTXs' clishared [] queuedTXs (initialAccounts, initialAccounts)
        where
          processTXs' :: CLISharedState -> [Transaction] -> TXQueue -> (PubKeyToAcc, PubKeyToAcc) -> IO ()
          processTXs' sharedState vTxs qTxs (accmap, fallback) = do
            when (length vTxs /= capacity) $ do
              tx <- atomically $ dequeueTQ qTxs
              if validateTransaction tx accmap
                then processTXs' sharedState (tx : vTxs) qTxs (updateAccsByTX tx accmap, fallback)
                else processTXs' sharedState vTxs qTxs (accmap, fallback)
            blockchain <- (readIORef . fst) sharedState
            (newaccs, newblock) <- mint (head blockchain) vTxs (accmap, fallback)
            writeIORef (fst sharedState) (newblock : blockchain)
            writeIORef (snd sharedState) (newaccs Map.! mypub)
            processTXs' sharedState [] qTxs (newaccs, newaccs)

      getValidatorBlock :: Block -> PublicKey -> IO Block
      getValidatorBlock = getValidatorBlockFrom queuedBlocks

      mint :: Block -> [Transaction] -> (PubKeyToAcc, PubKeyToAcc) -> IO (PubKeyToAcc, Block)
      mint = mint' myid (filter ((ip, port) /=) friends)

      mint' :: Int -> [Peer] -> Block -> [Transaction] -> (PubKeyToAcc, PubKeyToAcc) -> IO (PubKeyToAcc, Block)
      mint' selfID peers lastBlock vTxs (accountMap, fallback) = do
        let accs = Map.elems accountMap
            weights = zip (map accountStake accs) [1 .. length accs]

            prevhash = blockPreviousHash lastBlock
            seed = decode . BS.fromStrict $ prevhash

            validator = sampleValidator (mkStdGen seed) weights
            valkey = fst $ Map.elemAt validator accountMap

        if validator == selfID
          then do
            currtime <- getUnixTime
            let newBlock = createBlock (blockIndex lastBlock + 1) currtime vTxs valkey prevhash
                fees = txsFee $ blockTransactions lastBlock
                plusFees acc = acc {accountBalance = accountBalance acc + fees}
                newAccs = Map.update (Just . plusFees) valkey accountMap
            broadcastBlockTo peers newBlock
            return (newAccs, newBlock)
          else do
            -- spin on the queue of blocks until a valid one is found
            block <- getValidatorBlock lastBlock valkey
            let finalAccs = updateAccsByTXs (blockTransactions block) fallback
            return (finalAccs, block)

  -- spawn a thread to process transactions
  _ <- (liftIO . forkIO) processTXs
  liftIO $ runReaderT (shell clishared) cliinfo
  return ()