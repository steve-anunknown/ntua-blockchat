module Main(main) where

import OrdinaryNode
import BootstrapNode

import System.Directory (removeFile)
import Control.Concurrent
import Control.Monad (void)
import Network.Socket (HostName, ServiceName)

import qualified Data.List as List
import qualified Data.Map as Map

main :: IO ()
main = testSetup 100

testSetup :: Int -> IO ()
testSetup nodes = do
    let low      = 32590
        high     = low + nodes - 1
        hostip   = "127.0.0.1"
        hosts    = replicate nodes hostip
        ports    = map show [low .. high :: Int]
        infos    = zipWith NodeInfo hosts ports
        nodelogs = map (\n -> "node" ++ show n ++ ".log")  [1..nodes]

        boot    = BootInfo 1 hostip (show $ high + 1) nodes
        bootlog = "bootstrap.log"

        uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
        uncurry3 f (a, b, c) = f a b c

        logBootstrapNode :: MVar Int -> BootInfo -> String -> IO ()
        logBootstrapNode mvar info logfile = do
            void $ forkIO (do
                state <- bootstrapNode info
                let keys = List.sort $ bootPublicKeys state -- this is sorted to facilitate the comparison later
                    peers = bootPeers state :: [(HostName, ServiceName)]
                    nodeinfos = map (uncurry NodeInfo) peers
                    chain = bootBlockchain state
                writeFile logfile $ show (keys, nodeinfos, chain)
                putMVar mvar 1)

        logOrdinaryNode :: HostName -> ServiceName -> MVar Int -> NodeInfo -> String -> IO ()
        logOrdinaryNode bip bport mvar info logfile = do
            void $ forkIO (do
                state <- ordinaryNode bip bport info
                let accounts = nodeAccounts state
                    keys = Map.keys accounts -- the keys are sorted!! therefore the list previously must be sorted too
                writeFile logfile $ show (keys, nodePeers state, nodeBlockchain state)
                putMVar mvar 1)

    bootmvar  <- newEmptyMVar
    logBootstrapNode bootmvar boot bootlog -- spawn a thread for the bootstrapping node
    threadDelay 1500000 -- give the bootstrapping node time to start

    nodemvars <- mapM (const newEmptyMVar) [1..nodes]
    mapM_ (uncurry3 $ logOrdinaryNode hostip (bootNodePort boot)) (zip3 nodemvars infos nodelogs) -- spawn a thread for each ordinary node
    
    _ <- takeMVar bootmvar -- the main program waits for the bootstrapping to finish
    mapM_ takeMVar nodemvars -- the main program waits for the nodes to finish

    -- at this point, read the logs and make sure they are all equal to each other
    bootOutput <- readFile bootlog
    nodeOutputs <- mapM readFile nodelogs

    let allEqual = all (== bootOutput) nodeOutputs
    -- clean up the logs
    mapM_ removeFile (bootlog : nodelogs)

    putStrLn ""
    if allEqual then putStrLn "testSetup: Passed!" else putStrLn "testSetup: Failed!"



