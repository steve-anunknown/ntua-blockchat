module Main (main) where

import Test.Hspec
import System.Random ( mkStdGen )
import OrdinaryNode (sampleValidator, nodeStartupTest, NodeInfo(..))
import BootstrapNode (BootstrapNode(..), bootstrapNode, BootState(..), BootInfo(..))
import Wallet (generateWallet)
import Control.Concurrent (MVar, putMVar, forkIO, newEmptyMVar, threadDelay, takeMVar)
import Control.Monad (void, replicateM)
import Network.Simple.TCP (HostName, ServiceName)
import Control.Monad.Reader (runReaderT)
import System.Directory (removeFile)

main :: IO ()
main = hspec $ do
    describe "Sample Validator Tests" $ do
        it "returns a validator according to the given probabilities" $ do
            let g = mkStdGen 42  -- create a random number generator
                probs = [(0.5, 1), (0.5, 2)]  -- define the probabilities
                result = sampleValidator g probs
            result `shouldSatisfy` (`elem` [1, 2])

    describe "Node Startup Test" $ do
        it "checks if the node starts up correctly" $ do
            wallets <- replicateM 100 (generateWallet 512)
            let localhost = "127.0.0.1" :: HostName
                bootstrap = BootstrapNode localhost "35900"
                bootinfo = BootInfo 0 localhost "35900" 100
                -- nodes = map (NodeInfo localhost . show) [35901..35910]
                uncurry3 f (a, b, c) = f a b c
                nodes = map (uncurry3 NodeInfo) $ zip3 (repeat localhost) (map show [35901..36000 :: Int] :: [ServiceName]) wallets
                -- create logfiles node_i.log
                logs = map (("node_" ++) . show) [1..100 :: Int]

                logOrdinaryNode :: MVar Int -> NodeInfo -> String -> IO ()
                logOrdinaryNode mvar info logfile = void $ forkIO (do
                    (_, (keys, friends, genesis)) <- runReaderT (nodeStartupTest bootstrap) info
                    writeFile logfile $ show (keys, friends, [genesis])
                    putMVar mvar 1)

                logBootstrapNode :: MVar Int -> BootInfo -> String -> IO ()
                logBootstrapNode mvar info logfile = void $ forkIO (do
                    bootstate <- bootstrapNode info
                    writeFile logfile $ show (bootPublicKeys bootstate, bootPeers bootstate, bootBlockchain bootstate)
                    putMVar mvar 1)

            boottrigger <- newEmptyMVar
            logBootstrapNode boottrigger bootinfo "bootstrap.log"
            threadDelay 1000000 -- wait 1 second for the bootstrap node to start

            nodetriggers <- mapM (const newEmptyMVar) nodes
            mapM_ (\(mvar, info, logfile) -> logOrdinaryNode mvar info logfile) (zip3 nodetriggers nodes logs)

            _ <- takeMVar boottrigger
            mapM_ takeMVar nodetriggers
            
            bootstate <- readFile "bootstrap.log"
            nodestates <- mapM readFile logs
            -- remove the logs
            removeFile "bootstrap.log"
            mapM_ removeFile logs
            -- all of the states should be the same
            nodestates `shouldSatisfy` all (== bootstate)


