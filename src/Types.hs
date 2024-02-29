module Types
    (   Peer,
        PubKeyToAcc
    ) where

import qualified Data.Map as Map
import Account (Account)
import Codec.Crypto.RSA (PublicKey)
import Network.Simple.TCP (HostName, ServiceName)

type Peer = (HostName, ServiceName)

type PubKeyToAcc = Map.Map PublicKey Account