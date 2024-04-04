{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BootstrapNode (BootInfo (..), BootState (..), BootstrapNode (..), bootstrapNode)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (replicateM, void)
import Control.Monad.Reader (runReaderT)
import Network.Simple.TCP (HostName, ServiceName, connect, send, sendLazy, recv)
import OrdinaryNode (NodeInfo (..), nodeStartupTest, sampleValidator)
import System.Directory (removeFile)
import System.Random (mkStdGen)
import Test.Hspec
import Wallet (generateWallet)
import Utils (receiveChunks)
import Data.Maybe (fromMaybe)

main :: IO ()
main = hspec $ do
  describe "Sample Validator Tests" $ do
    it "returns a validator according to the given probabilities" $ do
      let g = mkStdGen 42 -- create a random number generator
          probs = [(0.5, 1), (0.5, 2)] -- define the probabilities
          result = sampleValidator g probs
      result `shouldSatisfy` (`elem` [1, 2])

  describe "receiveChunks strict" $ do
    it "returns the received bytes when the total length is less than the limit" $ do
      let limit = 10
      result <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        send socket "Hello"
        receiveChunks socket limit
      result `shouldBe` "Hello"

    it "returns the first 'limit' bytes when the total length is greater than the limit" $ do
      let limit = 5
      result <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        send socket "Hello, World!"
        receiveChunks socket limit
      result `shouldBe` "Hello"
    it "returns an empty ByteString when no bytes are received" $ do
      let limit = 10
      result <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        -- terminate the program that runs the server that echoes back the message
        -- or program into it some kind of timeout to exit
        receiveChunks socket limit
      result `shouldBe` ""
  describe "receiveChunks lazy" $ do
    it "returns the received bytes when the total length is less than the limit" $ do
      let limit = 10
      result <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        sendLazy socket "Hello"
        receiveChunks socket limit   
      result `shouldBe` "Hello"

    it "returns the first 'limit' bytes when the total length is greater than the limit" $ do
      let limit = 5
      result <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        sendLazy socket "Hello, World!"
        receiveChunks socket limit
      result `shouldBe` "Hello"
    it "returns an empty ByteString when no bytes are received" $ do
      let limit = 10
      result <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        -- terminate the program that runs the server that echoes back the message
        -- or program into it some kind of timeout to exit
        receiveChunks socket limit
      result `shouldBe` ""
    
  describe "receiveChunks against recv" $ do
    it "should return the same result as recv (msg less than limit)" $ do
      let limit = 10
      res1 <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        sendLazy socket "Hello"
        receiveChunks socket limit
      res2 <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        sendLazy socket "Hello"
        fromMaybe "" <$> recv socket limit
      res1 `shouldBe` res2
    it "should return the same result as recv (msg greater than limit)" $ do
      let limit = 5
      res1 <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        sendLazy socket "Hello, World!"
        receiveChunks socket limit
      res2 <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        sendLazy socket "Hello, World!"
        fromMaybe "" <$> recv socket limit
      res1 `shouldBe` res2
    it "should return the same result as recv (empty)" $ do
      let limit = 10
      res1 <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        -- terminate the program that runs the server that echoes back the message
        -- or program into it some kind of timeout to exit
        receiveChunks socket limit
      res2 <- connect "127.0.0.1" "35000" $ \(socket, _) -> do
        -- terminate the program that runs the server that echoes back the message
        -- or program into it some kind of timeout to exit
        fromMaybe "" <$> recv socket limit
      res1 `shouldBe` res2
    

  describe "Node Startup Test" $ do
    it "checks if the node starts up correctly" $ do
      wallets <- replicateM 100 (generateWallet 512)
      let localhost = "127.0.0.1" :: HostName
          bootstrap = BootstrapNode localhost "36900"
          bootinfo = BootInfo 0 localhost "36900" 100
          -- nodes = map (NodeInfo localhost . show) [35901..35910]
          uncurry3 f (a, b, c) = f a b c
          nodes = map (uncurry3 NodeInfo) $ zip3 (repeat localhost) (map show [36901 .. 37000 :: Int] :: [ServiceName]) wallets
          -- create logfiles node_i.log
          logs = map (("node_" ++) . show) [1 .. 100 :: Int]

          logOrdinaryNode :: MVar Int -> NodeInfo -> String -> IO ()
          logOrdinaryNode mvar info logfile =
            void $
              forkIO
                ( do
                    (_, (keys, friends, genesis)) <- runReaderT (nodeStartupTest bootstrap) info
                    writeFile logfile $ show (keys, friends, [genesis])
                    putMVar mvar 1
                )

          logBootstrapNode :: MVar Int -> BootInfo -> String -> IO ()
          logBootstrapNode mvar info logfile =
            void $
              forkIO
                ( do
                    bootstate <- bootstrapNode info
                    writeFile logfile $ show (bootPublicKeys bootstate, bootPeers bootstate, bootBlockchain bootstate)
                    putMVar mvar 1
                )

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

