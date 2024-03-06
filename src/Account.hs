module Account
  ( Account (..),
    initialAccount,
    availableBalance,
    updateBalanceBy,
    updateNonce,
    updateStake,
  )
where

data Account = Account
  { accountBalance :: Double, -- the balance of the account.
    accountNonce :: Int,
    -- a field that is incremented with every
    -- outgoing transaction, in order to guard
    -- against replay attacks.
    accountStake :: Double
    -- the amount of stake that the account has
    -- for the PoS protocol.
  }
  deriving (Show, Eq)

-- | The initial account has a balance of 1000 coins, a nonce of 0 and no stake.
initialAccount :: Account
initialAccount = Account 1000 0 0

-- | The available balance of an account is the balance minus the stake.
availableBalance :: Account -> Double
availableBalance Account {accountBalance = bal, accountStake = st} = bal - st

-- | This function takes an amount and an account as arguments and returns a new account
-- with the balance updated by the amount.
updateBalanceBy :: Double -> Account -> Account
updateBalanceBy amount acc = acc {accountBalance = accountBalance acc + amount}

-- | This function takes an account as an argument and returns a new account with the nonce
-- incremented by 1.
updateNonce :: Account -> Account
updateNonce acc = acc {accountNonce = accountNonce acc + 1}

-- | This function takes an amount and an account as arguments and returns a new account
-- with the stake updated by the amount.
updateStake :: Double -> Account -> Account
updateStake amount acc =
  acc
    { accountStake = amount,
      accountBalance = accountBalance acc + (accountStake acc - amount)
    }
