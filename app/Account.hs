module Account (
    Account(..),
    availableBalance
) where

import Wallet

data Account = Account {
    accountWallet :: Wallet,   -- the wallet associated with the account.
    accountBalance :: Double,  -- the balance of the account.
    accountNonce :: Int,       -- a field that is incremented with every
                               -- outgoing transaction, in order to guard
                               -- against replay attacks.
    accountStake :: Double     -- the amount of stake that the account has
                               -- for the PoS protocol.
}

availableBalance :: Account -> Double
availableBalance Account{accountBalance=bal, accountStake=st} = bal - bal * st
