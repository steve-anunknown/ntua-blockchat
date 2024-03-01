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
import Network.Simple.TCP (HostName, ServiceName, connect, send)
import ServiceType (ServiceType (..))
import System.IO (stdout)
import Transaction (Transaction (..), createTransaction)
import Utils (encodeStrict)
import Wallet (Wallet)

data CLIInfo = CLIInfo
  { cliWallet :: Wallet, -- Wallet of the user
    cliPeers :: Map.Map Int PublicKey, -- Peers of the user
    cliNodeIP :: HostName, -- Node IP of the user
    cliNodePort :: ServiceName -- Node Port of the user
  }
  deriving (Show)

type CLISharedState = (IORef Blockchain, IORef Account)

-- | Send a transaction to the network
sendTx :: Int -> ServiceType -> Account -> ReaderT CLIInfo IO ()
sendTx recvID service myacc = do
  ip <- asks cliNodeIP
  port <- asks cliNodePort
  peers <- asks cliPeers
  (pub, priv) <- asks cliWallet
  let mynonce = accountNonce myacc
      recvPub = Map.lookup recvID peers
  case recvPub of
    Nothing -> liftIO $ putStrLn "Invalid recipient. Check whether the ID was yours or if it does not exist."
    Just somekey -> do
      let tx = createTransaction pub somekey service mynonce priv
          msg = encodeStrict tx
      liftIO $ putStrLn $ "Sending " ++ show service ++ " to " ++ show recvID
      liftIO $ connect ip port $ \(sock, _) -> send sock msg

-- | Handle the input from the user
handle :: String -> CLISharedState -> ReaderT CLIInfo IO ()
handle input shared = do
  let tokens = words input
      (blockref, accref) = shared
  acc <- liftIO $ readIORef accref
  case tokens of
    ["t", num, "Coins", coins] -> sendTx (read num) (Coins (read coins)) acc
    ["t", num, "Message", msg] -> sendTx (read num) (Message msg) acc
    ["t", num, "Both", "(", coins, ",", msg, ")"] -> sendTx (read num) (Both (read coins, msg)) acc
    ["stake", coins] -> sendTx 0 (Coins (read coins)) acc
    ["view"] -> do
      blockchain <- liftIO $ readIORef blockref
      liftIO $ prettyPrintBlock (head blockchain)
    ["balance"] -> liftIO $ print (accountBalance acc)
    ["peers"] -> do
      peers <- asks cliPeers
      liftIO $ prettyPrintPeers peers
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

prettyPrintBlock :: Block -> IO ()
prettyPrintBlock block = do
  putStrLn $ "Block Index: " ++ show (blockIndex block)
  putStrLn $ "Block Timestamp: " ++ show (blockTimestamp block)
  putStrLn "Block Transactions: "
  prettyPrintTXs $ blockTransactions block
  putStrLn $ "Block Validator: " ++ show (blockValidator block)

prettyPrintTX :: Transaction -> IO ()
prettyPrintTX Transaction {senderAddress = s, receiverAddress = r, serviceType = st, nonce = n} = do
  putStrLn $ "Sender: " ++ show s
  putStrLn $ "Receiver: " ++ show r
  putStrLn $ "Service: " ++ show st
  putStrLn $ "Nonce: " ++ show n
  putStrLn "-------------------"

prettyPrintTXs :: [Transaction] -> IO ()
prettyPrintTXs = mapM_ prettyPrintTX

prettyPrintPeers :: Map.Map Int PublicKey -> IO ()
prettyPrintPeers = print . unlines . map (\(i, _) -> show "Node with ID " ++ show i) . Map.toList
