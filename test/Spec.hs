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
import Test.QuickCheck
    ( Gen,
      Arbitrary(arbitrary),
      Testable(property),
      listOf,
      suchThat,
      (==>),
      forAll )
import Test.Hspec ( hspec, describe, it, shouldSatisfy, shouldBe )
import Wallet (generateWallet)
import Utils (receiveChunks)
import Data.Maybe (fromMaybe)
import qualified Statistics.Sample as Statistics
import qualified Data.Vector as V

round4 :: Double -> Double
round4 x = fromIntegral (round $ x * 1e4) / 1e4

-- Custom generator for lists of Doubles greater than 0
positiveDoubles :: Gen [Double]
positiveDoubles = listOf (abs <$> arbitrary `suchThat` (> 0))

arithmetic :: Fractional a => [a] -> a
arithmetic numbers = totalsum / amount
    where (totalsum, amount) = foldr (\num (a, b) -> (num + a, b + 1)) (0, 0) numbers

harmonic :: Fractional a => [a] -> a
harmonic numbers = amount / inversesum
  where (inversesum, amount) = foldr (\num (a, b) -> (a + 1 / num, b + 1)) (0, 0) numbers

geometric :: Floating a => [a] -> a
geometric numbers = product numbers ** (1 / amount)
  where amount = fromIntegral $ length numbers

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

  describe "arithmeticMean" $ do
    it "returns the same result as Statistics.mean" $ property $
      \list -> not (null (list :: [Double])) ==>
        round4 (arithmetic list) `shouldBe` round4 (Statistics.mean (V.fromList list))

  describe "harmonicMean" $ do
    it "returns the same result as Statistics.harmonicMean" $ property $
      \list -> not (null (list :: [Double])) && notElem 0 list ==>
        round4 (harmonic list) `shouldBe` round4 (Statistics.harmonicMean (V.fromList list))

  describe "geometricMean" $ do
    it "returns the same result as Statistics.geometricMean" $ property $
      forAll positiveDoubles $ \list -> not (null list) ==>
        round4 (geometric list) `shouldBe` round4 (Statistics.geometricMean (V.fromList list))

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

