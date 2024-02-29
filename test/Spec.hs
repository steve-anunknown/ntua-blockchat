module Main (main) where

import Test.Hspec
import System.Random
import OrdinaryNode (sampleValidator)

main :: IO ()
main = hspec $ do
    describe "Sample Validator Tests" $ do
        it "returns a validator according to the given probabilities" $ do
            let g = mkStdGen 42  -- create a random number generator
                probs = [(0.5, 1), (0.5, 2)]  -- define the probabilities
                result = sampleValidator g probs
            result `shouldSatisfy` (`elem` [1, 2])
