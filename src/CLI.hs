{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( CLIInfo (..),
    CLISharedState,
    shell,
  )
where

import Account (Account (accountBalance, accountNonce))
import Block (Block (..), Blockchain)
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT, asks)
import Data.IORef (IORef, readIORef)
import qualified Data.Map as Map
import GHC.IO.Handle (hFlush)
import Network.Simple.TCP (HostName, ServiceName)
import ServiceType (ServiceType (..))
import System.IO (stdout)
import Transaction (Transaction (..), createTransaction, broadcastTransaction, zeropub)
import Wallet (Wallet)

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
  let tokens = words input
      (blockref, accref) = shared
  acc <- liftIO $ readIORef accref
  case tokens of
    ("t" : numStr : "Coins" : coinsStr : _) ->
      case (reads numStr, reads coinsStr) of
        ([(num, "")], [(coins, "")]) -> sendTx num (Coins coins) acc
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ("t" : numStr : "Message" : msgParts) ->
      case reads numStr of
        [(num, "")] -> sendTx num (Message $ unwords msgParts) acc
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ("t" : numStr : "Both" : coinsStr : "," : msgParts) ->
      case (reads numStr, reads coinsStr) of
        ([(num, "")], [(coins, "")]) -> sendTx num (Both (coins, unwords msgParts)) acc
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ("stake" : coinsStr : _) ->
      case reads coinsStr of
        [(coins, "")] -> stake coins acc
        _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."
    ["view"] -> do
      blockchain <- liftIO $ readIORef blockref
      prettyPrintBlock (head blockchain)
    ["balance"] -> liftIO $ print (accountBalance acc)
    ["peers"] -> do
      keymap <- asks cliIDtoKey
      liftIO $ prettyPrintPeers keymap
    ["help"] -> do
      liftIO $ putStrLn "t <recipient id> Coins <coins>          - send coins"
      liftIO $ putStrLn "t <recipient id> Message <msg>          - send message"
      liftIO $ putStrLn "t <recipient id> Both (<coins>, <msg>)  - send both"
      liftIO $ putStrLn "stake <coins>                           - stake coins"
      liftIO $ putStrLn "view                                    - view last block"
      liftIO $ putStrLn "balance                                 - view account balance"
      liftIO $ putStrLn "peers                                   - show list of peers"
      liftIO $ putStrLn "help                                    - show this message"
    _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."

-- | The main shell of the CLI
shell :: CLISharedState -> ReaderT CLIInfo IO ()
shell shared = do
  liftIO $ putStrLn "Welcome to the shell!"
  liftIO $ putStrLn "Type 'help' to ask for .. help."
  loop
  where
    loop :: ReaderT CLIInfo IO ()
    loop = do
      liftIO $ putStr "> " >> hFlush stdout
      input <- liftIO getLine
      handle input shared >> loop

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
