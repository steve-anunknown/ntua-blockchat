module Main (main) where

import BootstrapNode
import Control.Concurrent
import Control.Monad (void)
import qualified Data.List as List
import qualified Data.Map as Map
import Network.Socket (HostName, ServiceName)
import OrdinaryNode
import System.Directory (removeFile)

main :: IO ()
main = testSetup 100

testSetup :: Int -> IO ()
testSetup nodes = do
  let low = 32590
      high = low + nodes - 1
      hostip = "127.0.0.1"
      hosts = replicate nodes hostip
      ports = map show [low .. high :: Int]
      infos = zipWith NodeInfo hosts ports
      nodelogs = map (\n -> "node" ++ show n ++ ".log") [1 .. nodes]

      boot = BootInfo 1 hostip (show $ high + 1) nodes
      bootlog = "bootstrap.log"

      uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
      uncurry3 f (a, b, c) = f a b c

      logBootstrapNode :: MVar Int -> BootInfo -> String -> IO ()
      logBootstrapNode mvar info logfile = do
        void $
          forkIO
            ( do
                state <- bootstrapNode info
                let keys = List.sort $ bootPublicKeys state -- this is sorted to facilitate the comparison later
                    peers = bootPeers state :: [(HostName, ServiceName)]
                    nodeinfos = map (uncurry NodeInfo) peers
                    chain = bootBlockchain state
                writeFile logfile $ show (keys, nodeinfos, chain)
                putMVar mvar 1
            )

      logNode :: HostName -> ServiceName -> MVar Int -> NodeInfo -> String -> IO ()
      logNode bip bport mvar info logfile = do
        void $
          forkIO
            ( do
                state <- node bip bport info
                let accounts = nodeAccounts state
                    keys = Map.keys accounts -- the keys are sorted!! therefore the list previously must be sorted too
                writeFile logfile $ show (keys, nodePeers state, nodeBlockchain state)
                putMVar mvar 1
            )

  bootmvar <- newEmptyMVar
  logBootstrapNode bootmvar boot bootlog -- spawn a thread for the bootstrapping node
  -- give the bootstrapping node time to start
  threadDelay 1500000

  nodemvars <- mapM (const newEmptyMVar) [1 .. nodes]
  -- spawn a thread for each ordinary node
  mapM_ (uncurry3 $ logNode hostip (bootNodePort boot)) (zip3 nodemvars infos nodelogs)

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
