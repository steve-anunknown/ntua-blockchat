{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BootstrapNode
import Control.Monad (void)
import OrdinaryNode
import System.Environment
import System.Exit
import Wallet

main :: IO ()
main = getArgs >>= parseArgs >>= startDoingStuff >> exit

startDoingStuff :: [String] -> IO ()
startDoingStuff [num, host, port] = void $ bootstrapNode (BootInfo 0 host port nodes)
  where
    nodes = read num :: Int
startDoingStuff [host, port, bip, bport, capacity] = void $ do
  wallet <- generateWallet 2056
  node (BootstrapNode bip bport) cap (NodeInfo host port wallet)
  where
    cap = read capacity :: Int
startDoingStuff _ = usage >> exit

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

-- decodeStrict :: Binary a => BS.ByteString -> a
-- decodeStrict = decode . BS.fromStrict

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
  putStrLn "Usage: main --node <ip> <port> <bootstrap ip> <bootstrap port> <capacity>"
    >> putStrLn "       main --bootstrap <ip> <port> <num nodes>"

exit :: IO a
exit = exitSuccess
