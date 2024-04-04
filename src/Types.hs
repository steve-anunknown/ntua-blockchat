module Types
  ( Peer,
    PubKeyToAcc,
  )
where

import Account (Account)
import Codec.Crypto.RSA (PublicKey)
import qualified Data.Map as Map
import Network.Simple.TCP (HostName, ServiceName)

type Peer = (HostName, ServiceName)

type PubKeyToAcc = Map.Map PublicKey Account