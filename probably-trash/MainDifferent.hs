{-# LANGUAGE OverloadedStrings #-}

module MainDifferent (main) where

-- import Lib

import BootstrapNode
import OrdinaryNode
import Wallet
import Block
import Transaction

import Data.IORef
import Data.UnixTime
import Data.Binary (Binary, encode, decode)
import System.Environment
import System.Exit
import Control.Concurrent (forkFinally)
import Control.Monad (void, when)
import Control.Monad.Reader
import Network.Socket
import Network.Socket.ByteString (recv, sendAll, send, recv)
import qualified Control.Exception as E
import Codec.Crypto.RSA (PublicKey(..))

import qualified Data.ByteString as BS
import qualified Data.Map as Map

main :: IO ()
main = getArgs >>= parseArgs >>= startDoingStuff

startDoingStuff :: [String] -> IO ()
startDoingStuff [num, host, port] = bootstrapNode (BootInfo 0 host port nodes)
   where
     nodes = read num :: Int
startDoingStuff [host, port, bip, bport] = ordinaryNode bip bport (NodeInfo host port)
startDoingStuff _ = usage >> exit

bootstrapNode :: BootInfo -> IO ()
bootstrapNode = runReaderT bootDiffLogic

bootDiffLogic :: ReaderT BootInfo IO ()
bootDiffLogic = do
  info <- ask
  let myip       = bootNodeIP info
      myport     = bootNodePort info
      totalNodes = bootNodeNumb info

      updateState ::  (PublicKey, HostName, ServiceName) -> BootState -> (BootState, ())
      updateState (pub, ip, port) s = (s {bootCurrID = bootCurrID s + 1, bootFriends = Map.insert pub (ip, port) (bootFriends s)}, ())
      
      resolve = do
        let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just myip) (Just myport)

      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 5
        return sock

      serve :: IORef BootState -> Socket -> IO ()
      serve ioref sock = do
        st <- readIORef ioref
        putStrLn $ "Boot: " ++ show (bootCurrID st) ++ " <= " ++ show totalNodes
        putStrLn $ "Boot: " ++ show sock 
        when (bootCurrID st <= totalNodes) $ do
          E.bracketOnError (accept sock) (close . fst) serveIDs
          where
            serveIDs :: (Socket, SockAddr) -> IO ()
            serveIDs (conn, sockaddr) = do
              void $ forkFinally serveIDs' (const $ gracefulClose sock 5000)
              where
                  serveIDs' :: IO ()
                  serveIDs' = do
                    putStrLn $ "Boot Accepted Connection: " ++ show (conn, sockaddr)
                    msg <- recv conn 4096 -- receive public key and address
                    st  <- readIORef ioref
                    let (pub, ip, port) = decodeStrict msg :: (PublicKey, HostName, ServiceName)
                    sendAll conn $ encodeStrict $ bootCurrID st
                    atomicModifyIORef ioref $ updateState (pub, ip, port)
                    gracefulClose conn 5000
                    serve ioref sock -- this loops

  liftIO $ putStrLn $ "Starting bootstrap node at " ++ myip ++ ":" ++ myport
  state <- liftIO $ newIORef $ BootState 1 Map.empty []
  addr  <- liftIO resolve
  liftIO $ E.bracket (open addr) close $ serve state
  return ()



ordinaryNode :: HostName -> ServiceName -> NodeInfo -> IO ()
ordinaryNode bip bport = runReaderT $ ordinaryDiffLogic bip bport

ordinaryDiffLogic :: HostName -> ServiceName -> ReaderT NodeInfo IO ()
ordinaryDiffLogic bip bport = do
  info <- ask
  (pub, _) <- liftIO $ generateWallet 2048
  let ip   = nodeIP info
      port = nodePort info
      msg  = encodeStrict (pub, ip, port)

      resolve = do
        let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just bip) (Just bport)

      open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

      getIDfromBoot :: IORef NodeState -> Socket -> IO()
      getIDfromBoot ioref sock = do
        sendAll sock msg
        resp <- recv sock 32
        atomicModifyIORef ioref $ \s -> (s {nodeID = decodeStrict resp}, ())


  liftIO $ putStrLn $ "Starting ordinary node at " ++ ip ++ ":" ++ port
  state <- liftIO $ newIORef (NodeState 0 Map.empty [])
  addr  <- liftIO resolve
  liftIO $ E.bracket (open addr) close $ getIDfromBoot state
  return ()


----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

decodeStrict :: Binary a => BS.ByteString -> a
decodeStrict = decode . BS.fromStrict

encodeStrict :: Binary a => a -> BS.ByteString
encodeStrict = BS.toStrict . encode

decodeMaybe :: Binary a => Maybe BS.ByteString -> a
decodeMaybe = maybe (decode "") (decode . BS.fromStrict)

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
