{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Lib
import Network.Simple.TCP
import System.Environment
import System.Exit
import Control.Monad.Reader
import BootstrapNode
import OrdinaryNode

main :: IO ()
main = getArgs >>= parseArgs >>= startDoingStuff

startDoingStuff :: [String] -> IO ()
startDoingStuff [num, host, port] = bootstrapNode (BootstrapInfo 0 host port nodes)
   where
     nodes = read num :: Int
startDoingStuff [host, port, bip, bport] = ordinaryNode bip bport (NodeInfo 1 host port)
startDoingStuff _ = usage >> exit

bootstrapNode :: BootstrapInfo -> IO ()
bootstrapNode = runReaderT bootstrapNodeLogic

bootstrapNodeLogic :: ReaderT BootstrapInfo IO ()
bootstrapNodeLogic = do
  info <- ask
  let ip = bootNodeIP info
      port = bootNodePort info
      host = Host ip
  liftIO $ putStrLn $ "Starting bootstrap node at " ++ ip ++ ":" ++ port
  _ <- liftIO $ serve host port $ \(socket, addr) -> do
    putStrLn $ "BT: TCP connection established from " ++ show addr
    send socket "BT: Hello!"
    msg <- recv socket 1024
    putStrLn $ "BT: Received: " ++ show msg
    send socket "BT: Goodbye!"
    putStrLn "BT: Closing connection"
    closeSock socket
  return ()

-- TODO: implement the logic for ordinaryNode
ordinaryNode :: HostName -> ServiceName -> NodeInfo -> IO ()
ordinaryNode bip bport = runReaderT $ ordinaryNodeLogic bip bport

ordinaryNodeLogic :: HostName -> ServiceName -> ReaderT NodeInfo IO ()
ordinaryNodeLogic bip bport = do
  info <- ask
  let ip = nodeIP info
      port = nodePort info
  liftIO $ putStrLn $ "Starting ordinary node at " ++ ip ++ ":" ++ port
  _ <- liftIO $ connect bip bport $ \(socket,_) -> do
    putStrLn "ORD: Connected to the bootstrap node"
    send socket "ORD: Hello!"
    msg <- recv socket 1024
    putStrLn $ "ORD: Received: " ++ show msg
    send socket "ORD: Goodbye!"
    putStrLn "ORD: Closing connection"
    closeSock socket
  return ()

parseArgs :: [String] -> IO [String]
parseArgs ("--node" : restArgs) = help restArgs
  where
    help :: [String] -> IO [String]
    help args | length args == 4 = return args
    help _ = usage >> exit
parseArgs ("--bootstrap" : restArgs) = help restArgs
  where
    help :: [String] -> IO [String]
    help args | length args == 3 = return args
    help _ = usage >> exit
parseArgs _ = usage >> exit

usage :: IO ()
usage =
  putStrLn "Usage: main --node <ip> <port> <bootstrap ip> <bootstrap port>"
    >> putStrLn "       main --bootstrap <ip> <port> <num nodes>"

exit :: IO a
exit = exitSuccess
