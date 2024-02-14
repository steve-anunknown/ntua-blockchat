module Main(main) where

import OrdinaryNode
import BootstrapNode

import System.Directory (removeFile)
import Control.Concurrent
import Control.Monad (void)
import Network.Socket (HostName, ServiceName)

main :: IO ()
main = testSetup 10

testSetup :: Int -> IO ()
testSetup nodes = do
    let 
        low      = 32590
        high     = low + nodes - 1
        hostip   = "192.168.1.9"
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
                (friends, chain) <- bootstrapNode info
                writeFile logfile $ show (friends, chain)
                putMVar mvar 1)

        logOrdinaryNode :: HostName -> ServiceName -> MVar Int -> NodeInfo -> String -> IO ()
        logOrdinaryNode bip bport mvar info logfile = do
            void $ forkIO (do
                (friends, chain) <- ordinaryNode bip bport info
                writeFile logfile $ show (friends, chain)
                putMVar mvar 1)

    bootmvar  <- newEmptyMVar
    nodemvars <- mapM (const newEmptyMVar) [1..nodes]
    logBootstrapNode bootmvar boot bootlog -- spawn a thread for the bootstrapping node
    mapM_ (uncurry3 $ logOrdinaryNode (bootNodeIP boot) (bootNodePort boot)) (zip3 nodemvars infos nodelogs) -- spawn a thread for each ordinary node
    
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



