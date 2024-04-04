{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OrdinaryNode
  ( NodeInfo (..),
    node,
    sampleValidator, -- export it just for testing
    nodeStartupTest, -- export it just for testing
  )
where

import Account
  ( Account (accountBalance, accountNonce, accountStake),
    initialAccount,
  )
import Block (Block (..), broadcastBlock, createBlock, emptyBlock, txIsUnique, validateBlock)
import BootstrapNode (BootstrapNode (..))
import CLI (CLIInfo (..), CLISharedState, shell)
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
import Data.Binary (decode, decodeOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
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
import Utils (decodeMaybe, encodeStrict, receiveChunks)
import Wallet (Wallet)

type TXQueue = TQueue Transaction

type BLQueue = TQueue Block

type StartupState = ([(Int, PublicKey)], [Peer], Block)

-- NodeInfo does not change.
-- It is set once, using the arguments passed to the program and then remains constant.
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

server :: ServerEnv -> (Socket, SockAddr) -> IO ()
server (ServerEnv startupState trigger queuedTxs queuedBlocks) (socket, _) = do
  resp <- receiveChunks socket $ 16 * 4096
  let (msgtype, msg) = BS.splitAt 1 resp
  case msgtype of
    "0" -> handleDecodeStartup startupState trigger msg
    "1" -> handleDecodeTx queuedTxs msg
    "2" -> handleDecodeBlock queuedBlocks msg
    _ -> liftIO $ putStrLn "This should not be seen"

-- The following are debugging wrappers. The logic could be inlined above
-- in the server, but the wrappers are helpful for debugging, in case
-- something goes wrong.
handleDecodeStartup :: TVar StartupState -> MVar Int -> BS.ByteString -> IO ()
handleDecodeStartup startupState trigger msg =
  case decodeOrFail (LBS.fromStrict msg) of
    Left (_, _, errmsg) -> putStrLn $ "From startup: " ++ errmsg
    Right (_, _, result) -> do
      atomically $ writeTVar startupState result
      putMVar trigger 1

handleDecodeTx :: TXQueue -> BS.ByteString -> IO ()
handleDecodeTx queue msg =
  case decodeOrFail (LBS.fromStrict msg) of
    Left (_, _, errmsg) -> putStrLn $ "From tx: " ++ errmsg
    Right (_, _, tx) -> atomically $ enqueueTQ queue tx

handleDecodeBlock :: BLQueue -> BS.ByteString -> IO ()
handleDecodeBlock queue msg =
  case decodeOrFail (LBS.fromStrict msg) of
    Left (_, _, errmsg) -> putStrLn $ "From block: " ++ errmsg
    Right (_, _, block) -> atomically $ enqueueTQ queue block

-- | This function simulates the lottery for the validator.
sampleValidator :: (RandomGen g) => g -> [(Double, Int)] -> Int
sampleValidator g probs = evalState (sampleStateRVar (weightedCategorical epsProbs)) g
  where
    epsilon = 0.0001 :: Double
    epsProbs = map (\(x, y) -> (x + epsilon, y)) probs

-- | Helper function to get a valid block from a queue of blocks
--  given the last block and the public key of the validator.
getValidatorBlockFrom :: BLQueue -> Block -> PublicKey -> IO Block
getValidatorBlockFrom qBlocks lastBlock valKey = do
  block <- atomically $ dequeueTQ qBlocks
  if validateBlock block lastBlock valKey
    then return block
    else getValidatorBlockFrom qBlocks lastBlock valKey

-- | This function is called from the driver to start the node.
node :: BootstrapNode -> Int -> NodeInfo -> IO ()
node bootstrap capacity = runReaderT $ nodeLogic bootstrap capacity

-- | This function connects to the bootstrap node and gets the id of the node.
nodeGetID :: BootstrapNode -> ReaderT NodeInfo IO Int
nodeGetID (BootstrapNode bip bport) = do
  ip <- asks nodeIP
  port <- asks nodePort
  (pub, _) <- asks nodeInfoWallet
  let msg = encodeStrict (pub, ip, port)
      getIDfromBoot :: (Socket, SockAddr) -> IO Int
      getIDfromBoot (socket, _) = do
        send socket msg
        resp <- recv socket 32 -- for some reason the startup phase gets stuck
        return $ decodeMaybe resp -- if 'receiveChunks' is used instead of recv
  liftIO $ connect bip bport getIDfromBoot

-- | This function is the main logic of the node.
-- It sets up the node and then starts the server that
-- also implements the PoS protocol.
nodeLogic :: BootstrapNode -> Int -> ReaderT NodeInfo IO ()
nodeLogic bootstrap capacity = do
  -- start by setting up the startup state.
  ip <- asks nodeIP
  port <- asks nodePort
  myid <- nodeGetID bootstrap -- connect to bootstrap and get id
  liftIO $ putStrLn $ "My id is: " ++ show myid
  mywallet@(mypub, _) <- asks nodeInfoWallet -- used in processTXs
  trigger <- liftIO newEmptyMVar -- initialize trigger for broadcast
  queuedTXs <- liftIO (newTQueueIO :: IO TXQueue)
  queuedBlocks <- liftIO (newTQueueIO :: IO BLQueue)
  startupState <- liftIO $ newTVarIO ([] :: [(Int, PublicKey)], [] :: [Peer], emptyBlock)
  -- setup a server that never returns. it discriminates between
  -- the types of messages it receives and acts accordingly.
  let env = ServerEnv startupState trigger queuedTXs queuedBlocks
  _ <- liftIO . forkIO $ serve (Host ip) port $ server env
  _ <- liftIO $ takeMVar trigger -- wait for the broadcast to finish
  (keys, friends, genesis) <- liftIO $ readTVarIO startupState

  -- Up to this point, the broadcast has finished. Each node has its own id,
  -- a list of peers and the genesis block. Now we can start the node.

  -- create references for the shared state
  blockchainRef <- liftIO $ newIORef [genesis]
  accountRef <- liftIO $ newIORef initialAccount
  let initialAccounts = Map.fromList $ map ((,initialAccount) . snd) keys
      clishared = (blockchainRef, accountRef) :: CLISharedState
      mypeers = filter (/= (myid, mypub)) keys
      cliinfo = CLIInfo mywallet (Map.fromList mypeers) ip port friends
      -- This function processes transactions (keeping track of the counter) and mints
      -- a new block when the counter reaches the capacity.
      processTXs :: IO ()
      processTXs = processTXs' clishared [] queuedTXs (initialAccounts, initialAccounts)
        where
          processTXs' :: CLISharedState -> [Transaction] -> TXQueue -> (PubKeyToAcc, PubKeyToAcc) -> IO ()
          processTXs' sharedState vTxs qTxs (accmap, fallback) = do
            when (length vTxs /= capacity) $ do
              tx <- atomically $ dequeueTQ qTxs
              blockchain <- (readIORef . fst) sharedState
              if validateTransaction tx accmap && txIsUnique tx blockchain
                then
                  processTXs' sharedState (tx : vTxs) qTxs (updateAccsByTX tx accmap, fallback)
                else
                  processTXs' sharedState vTxs qTxs (accmap, fallback)
            blockchain <- (readIORef . fst) sharedState
            (newblock, newaccs) <- mint (head blockchain) vTxs (accmap, fallback)
            writeIORef (fst sharedState) (newblock : blockchain)
            -- the cli is responsible for updating the account nonce
            atomicModifyIORef' (snd sharedState) (\a -> ((newaccs Map.! mypub) {accountNonce = accountNonce a}, ()))
            processTXs' sharedState [] qTxs (newaccs, newaccs)

      getValidatorBlock :: Block -> PublicKey -> IO Block
      getValidatorBlock = getValidatorBlockFrom queuedBlocks

      mint :: Block -> [Transaction] -> (PubKeyToAcc, PubKeyToAcc) -> IO (Block, PubKeyToAcc)
      mint = mint' (filter ((ip, port) /=) friends)

      mint' :: [Peer] -> Block -> [Transaction] -> (PubKeyToAcc, PubKeyToAcc) -> IO (Block, PubKeyToAcc)
      mint' peers lastBlock vTxs (accountMap, fallback) = do
        let -- the stake has to be taken from the fallback state
            -- because subsequent states are not validated
            accs = Map.elems fallback
            weights = zip (map accountStake accs) [1 ..]

            prevhash = blockPreviousHash lastBlock
            currhash = blockCurrentHash lastBlock
            seed = decode . BS.fromStrict $ prevhash

            validator = sampleValidator (mkStdGen seed) weights
            valkey = fst $ Map.elemAt (validator - 1) accountMap
        if valkey == mypub
          then do
            currtime <- getUnixTime
            let newBlock = createBlock (blockIndex lastBlock + 1) currtime (reverse vTxs) valkey currhash
                fees = txsFee vTxs
                plusFees acc = acc {accountBalance = accountBalance acc + fees}
                newAccs = Map.update (Just . plusFees) valkey accountMap
            broadcastBlock peers newBlock
            return (newBlock, newAccs)
          else do
            -- spin on the queue of blocks until a valid one is found
            block <- getValidatorBlock lastBlock valkey
            let finalAccs = updateAccsByTXs (blockTransactions block) fallback
            return (block, finalAccs)

  -- spawn a thread to process transactions
  _ <- (liftIO . forkIO) processTXs
  liftIO $ runReaderT (shell clishared) cliinfo

-- | This function is used to test the node startup. It returns the id of the node
-- and the startup state.
nodeStartupTest :: BootstrapNode -> ReaderT NodeInfo IO (Int, StartupState)
nodeStartupTest bootstrap = do
  -- start by setting up the startup state.
  ip <- asks nodeIP
  port <- asks nodePort
  myid <- nodeGetID bootstrap -- connect to bootstrap and get id
  trigger <- liftIO newEmptyMVar -- initialize trigger for broadcast
  queuedTXs <- liftIO (newTQueueIO :: IO TXQueue)
  queuedBlocks <- liftIO (newTQueueIO :: IO BLQueue)
  startupState <- liftIO $ newTVarIO ([] :: [(Int, PublicKey)], [] :: [Peer], emptyBlock)

  -- setup a server that never returns. it discriminates between
  -- the types of messages it receives and acts accordingly.
  _ <- liftIO . forkIO $ serve (Host ip) port $ server (ServerEnv startupState trigger queuedTXs queuedBlocks)
  _ <- liftIO $ takeMVar trigger -- wait for the broadcast to finish
  (keys, friends, genesis) <- liftIO $ readTVarIO startupState
  return (myid, (keys, friends, genesis))
