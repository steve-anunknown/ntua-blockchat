module CLI
  ( CLIInfo (..),
    CLISharedState,
    shell,
  )
where

import Account (Account (accountBalance, accountNonce))
import Block (Blockchain)
import Codec.Crypto.RSA (PublicKey)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT,
  )
import Data.IORef (IORef, readIORef)
import qualified Data.Map as Map
import Network.Simple.TCP (HostName, ServiceName, connect, send)
import ServiceType (ServiceType (..))
import Transaction (createTransaction)
import Utils (encodeStrict)
import Wallet (Wallet)

data CLIInfo = CLIInfo
  { cliWallet :: Wallet, -- Wallet of the user
    cliPeers :: Map.Map Int PublicKey, -- Peers of the user
    cliNodeIP :: HostName, -- Node IP of the user
    cliNodePort :: ServiceName -- Node Port of the user
  }

type CLISharedState = (IORef Blockchain, IORef Account)

-- function that basically communicates with the backend
-- that is the node
sendTx :: Int -> ServiceType -> Account -> ReaderT CLIInfo IO ()
sendTx recvID service myacc = do
  info <- ask
  let (pub, priv) = cliWallet info
      mynonce = accountNonce myacc
      recvPub = Map.lookup recvID $ cliPeers info
  case recvPub of
    Nothing -> liftIO $ putStrLn "Invalid recipient. Check whether the ID was yours or if it does not exist."
    Just somekey -> do
      let tx = createTransaction pub somekey service mynonce priv
          msg = encodeStrict tx
      liftIO $ putStrLn $ "Sending " ++ show service ++ " to " ++ show recvID
      liftIO $ connect (cliNodeIP info) (cliNodePort info) $ \(sock, _) ->
        send sock msg

-- handle user input
-- note: the ReaderT environment is just passed to the sendTx function
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
      lastblock <- liftIO $ readIORef blockref
      liftIO $ print lastblock
    ["balance"] -> liftIO $ print (accountBalance acc)
    ["help"] -> do
      liftIO $ putStrLn "t <recipient id> Coins <coins>          - send coins"
      liftIO $ putStrLn "t <recipient id> Message <msg>          - send message"
      liftIO $ putStrLn "t <recipient id> Both (<coins>, <msg>)  - send both"
      liftIO $ putStrLn "stake <coins>                           - stake coins"
      liftIO $ putStrLn "view                                    - view last block"
      liftIO $ putStrLn "balance                                 - view account balance"
      liftIO $ putStrLn "help                                    - show this message"
    _ -> liftIO $ putStrLn "Invalid command. Try entering 'help' for help."

-- main loop
-- the shared state will be created from whoever calls this function
-- probably from the main function.
shell :: CLISharedState -> ReaderT CLIInfo IO ()
shell shared = do
  liftIO $ putStrLn "Welcome to the shell!"
  liftIO $ putStrLn "Type 'help' to ask for .. help."
  loop
  where
    loop :: ReaderT CLIInfo IO ()
    loop = do
      liftIO $ putStr "> "
      input <- liftIO getLine
      handle input shared >> loop
