{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( CLIInfo (..),
    CLISharedState,
    shell,
  )
where

import Account (Account (accountBalance, accountNonce))
import Block (Block (..), Blockchain, meanBlockTime)
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT, asks)
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import qualified Data.Map as Map
import GHC.IO.Handle (hFlush)
import Network.Simple.TCP (HostName, ServiceName)
import ServiceType (ServiceType (..))
import System.IO (stdout)
import Transaction (Transaction (..), createTransaction, broadcastTransaction, zeropub)
import Wallet (Wallet)
import Control.Exception (IOException, catch, throwIO)
import System.IO.Error (isEOFError)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

data CLIInfo = CLIInfo
  { cliWallet :: Wallet, -- Wallet of the user
    cliIDtoKey :: Map.Map Int PublicKey, -- Peers of the user
    cliNodeIP :: HostName, -- Node IP of the user
    cliNodePort :: ServiceName, -- Node Port of the user
    cliPeers :: [(HostName, ServiceName)]
  }
  deriving (Show)

type CLISharedState = (IORef Blockchain, IORef Account)

-- | Send a transaction to the network
sendTx :: Int -> ServiceType -> Account -> ReaderT CLIInfo IO ()
sendTx recvID service myacc = do
  peers <- asks cliPeers
  keymap <- asks cliIDtoKey
  (pub, priv) <- asks cliWallet
  let mynonce = accountNonce myacc
      recvPub = Map.lookup recvID keymap
  case recvPub of
    Nothing -> liftIO $ putStrLn "Invalid recipient. Check whether the ID was yours or if it does not exist."
    Just somekey -> do
      let tx = createTransaction pub somekey service mynonce priv
      liftIO $ putStrLn $ "Sending " ++ show service ++ " to " ++ show recvID
      liftIO $ broadcastTransaction peers tx -- this handles the correct sending

-- | Send a staking transaction to the network
stake :: Double -> Account -> ReaderT CLIInfo IO () 
stake coins myacc = do
  peers <- asks cliPeers
  (pub, priv) <- asks cliWallet
  let mynonce = accountNonce myacc
      tx = createTransaction pub zeropub (Coins coins) mynonce priv
  liftIO $ putStrLn $ "Staking " ++ show coins
  liftIO $ broadcastTransaction peers tx

-- | Handle the input from the user
handle :: String -> CLISharedState -> ReaderT CLIInfo IO ()
handle input shared = do
  liftIO $ threadDelay 500000
  let tokens = words input
      (blockref, accref) = shared
  case tokens of
    ("t" : numStr : "Coins" : coinsStr : _) ->
      case (reads numStr, reads coinsStr) of
        ([(num, "")], [(coins, "")]) -> do
          liftIO $ atomicModifyIORef' accref (\a -> (a {accountNonce = accountNonce a + 1}, ()))
          acc' <- liftIO $ readIORef accref
          sendTx num (Coins coins) acc'
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ("t" : numStr : "Message" : msgParts) ->
      case reads numStr of
        [(num, "")] -> do
          liftIO $ atomicModifyIORef' accref (\a -> (a {accountNonce = accountNonce a + 1}, ()))
          acc' <- liftIO $ readIORef accref
          sendTx num (Message $ unwords msgParts) acc'
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ("t" : numStr : "Both" : coinsStr : "," : msgParts) ->
      case (reads numStr, reads coinsStr) of
        ([(num, "")], [(coins, "")]) -> do
          liftIO $ atomicModifyIORef' accref (\a -> (a {accountNonce = accountNonce a + 1}, ()))
          acc' <- liftIO $ readIORef accref
          sendTx num (Both (coins, unwords msgParts)) acc'
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ("stake" : coinsStr : _) ->
      case reads coinsStr of
        [(coins, "")] -> do
          liftIO $ atomicModifyIORef' accref (\a -> (a {accountNonce = accountNonce a + 1}, ()))
          acc' <- liftIO $ readIORef accref
          stake coins acc'
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ["view"] -> do
      blockchain <- liftIO $ readIORef blockref
      prettyPrintBlock (head blockchain)
    ["blockchain"] -> do
      liftIO $ threadDelay 5000000 -- just for testing
      blockchain <- liftIO $ readIORef blockref
      prettyPrintBlockchain blockchain
      -- show the mean time between blocks but disregard the genesis block
      -- that is the last one in the list
      liftIO $ putStrLn $ "Mean time between blocks: " ++ show (meanBlockTime $ init blockchain)
    ["balance"] -> do
      acc <- liftIO $ readIORef accref
      liftIO $ print (accountBalance acc)
    ["peers"] -> do
      keymap <- asks cliIDtoKey
      liftIO $ prettyPrintPeers keymap
    ["load", filename] -> do 
      liftIO $ putStrLn $ "Loading transactions from " ++ filename
      -- execute each line of the file as a command
      contents <- liftIO $ readFile filename
      mapM_ (`CLI.handle` shared) (lines contents)
    ["exit"] -> liftIO $ putStrLn "Exiting .." >> exitSuccess
    ["help"] -> do
      liftIO $ putStrLn "t <recipient id> Coins <coins>          - send coins"
      liftIO $ putStrLn "t <recipient id> Message <msg>          - send message"
      liftIO $ putStrLn "t <recipient id> Both (<coins>, <msg>)  - send both"
      liftIO $ putStrLn "stake <coins>                           - stake coins"
      liftIO $ putStrLn "view                                    - view last block"
      liftIO $ putStrLn "blockchain                              - view the entire blockchain"
      liftIO $ putStrLn "balance                                 - view account balance"
      liftIO $ putStrLn "peers                                   - show list of peers"
      liftIO $ putStrLn "load <filename>                         - load transactions from a file"
      liftIO $ putStrLn "exit                                    - exit the shell"
      liftIO $ putStrLn "help                                    - show this message"
    _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."

-- | The main shell of the CLI
shell :: CLISharedState -> ReaderT CLIInfo IO ()
shell shared = do
  liftIO $ putStrLn "Loading .."
  liftIO $ threadDelay 2000000
  liftIO $ putStrLn "Welcome to (the s)hell!"
  liftIO $ putStrLn "Type 'help' to ask for help."
  loop
  where
    loop :: ReaderT CLIInfo IO ()
    loop = do
      liftIO $ putStr "> " >> hFlush stdout
      input <- liftIO safeGetLine
      case input of
        Nothing -> CLI.handle "exit" shared
        Just line -> CLI.handle line shared >> loop

-- | Function to safely attempt reading a line, returning Nothing on EOF
safeGetLine :: IO (Maybe String)
safeGetLine = fmap Just getLine `catch` eofHandler
  where
    eofHandler :: IOException -> IO (Maybe String)
    eofHandler e
      | isEOFError e = return Nothing
      | otherwise = throwIO e

prettyPrintBlockchain :: Blockchain -> ReaderT CLIInfo IO ()
prettyPrintBlockchain = mapM_ prettyPrintBlock . reverse

prettyPrintBlock :: Block -> ReaderT CLIInfo IO ()
prettyPrintBlock block = do
  idTokey <- asks cliIDtoKey
  -- use the validator key to get its ID
  let matches = Map.keys $ Map.filter (== blockValidator block) idTokey
      validatorID = if null matches then "you" else show $ head matches
  liftIO $ putStrLn "==================================================="
  liftIO $ putStrLn $ "Block Index: " ++ show (blockIndex block)
  liftIO $ putStrLn $ "Block Timestamp: " ++ show (blockTimestamp block)
  liftIO $ putStrLn "Block Transactions: "
  prettyPrintTXs $ blockTransactions block
  liftIO $ putStrLn $ "Block Validator: " ++ (if blockIndex block == 1 then "genesis" else validatorID)
  liftIO $ putStrLn "==================================================="

prettyPrintTX :: Transaction -> ReaderT CLIInfo IO ()
prettyPrintTX Transaction {senderAddress = s, receiverAddress = r, serviceType = st, nonce = n} = do
  idToKey <- asks cliIDtoKey
  let matchesS = Map.keys $ Map.filter (== s) idToKey
      matchesR = Map.keys $ Map.filter (== r) idToKey
      sid = if null matchesS then "you" else show $ head matchesS
      rid = if null matchesR then "you" else show $ head matchesR
  liftIO $ putStrLn "\t|---------------------------------------------------"
  liftIO $ putStrLn $ "\t|TX Sender: " ++ (if s == zeropub then "genesis" else sid)
  liftIO $ putStrLn $ "\t|TX Receiver: " ++ (if s == zeropub then "bootstrap" else (if r == zeropub then "stake" else rid))
  liftIO $ putStrLn $ "\t|TX Service: " ++ show st
  liftIO $ putStrLn $ "\t|TX Nonce: " ++ show n
  liftIO $ putStrLn "\t|---------------------------------------------------"

prettyPrintTXs :: [Transaction] -> ReaderT CLIInfo IO ()
prettyPrintTXs = mapM_ prettyPrintTX

prettyPrintPeers :: Map.Map Int PublicKey -> IO ()
prettyPrintPeers = print . unlines . map (\(i, _) -> "Node with ID " ++ show i) . Map.toList
