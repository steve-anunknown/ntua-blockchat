{-# LANGUAGE PackageImports #-}

module Wallet (
    Wallet (..),
    generateWallet
) where

import Codec.Crypto.RSA (PublicKey, PrivateKey, generateKeyPair)
import "crypto-api" Crypto.Random    (newGenIO, SystemRandom)

data Wallet = Wallet (PublicKey, PrivateKey)

-- these returns an IO pair, so in order to actually use
-- the values, we have to be inside of something
-- that executes an IO action
generateWallet :: Int -> IO (PublicKey, PrivateKey)
generateWallet bits = do
    g <- newGenIO :: IO SystemRandom
    let (pubKey, privKey, _) = generateKeyPair g bits
    return (pubKey, privKey)

