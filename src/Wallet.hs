{-# LANGUAGE PackageImports #-}

module Wallet
  ( Wallet,
    emptyWallet,
    generateWallet,
  )
where

import Codec.Crypto.RSA (PrivateKey, PublicKey, generateKeyPair)
import "crypto-api" Crypto.Random (SystemRandom, newGenIO)

type Wallet = (PublicKey, PrivateKey)

emptyWallet :: Wallet
emptyWallet = (undefined, undefined)

-- these returns an IO pair, so in order to actually use
-- the values, we have to be inside of something
-- that executes an IO action
generateWallet :: Int -> IO (PublicKey, PrivateKey)
generateWallet bits = do
  g <- newGenIO :: IO SystemRandom
  let (pubKey, privKey, _) = generateKeyPair g bits
  return (pubKey, privKey)
