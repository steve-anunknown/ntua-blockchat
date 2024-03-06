{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BootstrapNode
  ( BootInfo (BootInfo),
    BootstrapNode (BootstrapNode),
    bootstrapNode,
  )
import Control.Monad (void)
import OrdinaryNode (NodeInfo (NodeInfo), node)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Wallet (generateWallet)

main :: IO ()
main = getArgs >>= parseArgs >>= startDoingStuff >> exit

startDoingStuff :: [String] -> IO ()
startDoingStuff [host, port, bip, bport, capacity] = void $ do
  wallet <- generateWallet 2056
  node (BootstrapNode bip bport) (read capacity) (NodeInfo host port wallet)
startDoingStuff [host, port, num] = void $ bootstrapNode (BootInfo 0 host port nodes)
  where
    nodes = read num :: Int
startDoingStuff _ = usage >> exit

parseArgs :: [String] -> IO [String]
parseArgs ("--node" : restArgs) = help restArgs
  where
    help :: [String] -> IO [String]
    help args | length args == 5 = return args
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
