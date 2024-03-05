module Account
  ( Account (..),
    initialAccount,
    availableBalance,
    updateBalanceBy,
    updateNonce,
    updateStakeBy
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

initialAccount :: Account
initialAccount = Account 1000 0 0

availableBalance :: Account -> Double
availableBalance Account {accountBalance = bal, accountStake = st} = bal - st

updateBalanceBy :: Double -> Account -> Account
updateBalanceBy amount acc  = acc {accountBalance = accountBalance acc + amount}

updateNonce :: Account -> Account
updateNonce acc = acc {accountNonce = accountNonce acc + 1}

updateStakeBy :: Double -> Account -> Account
updateStakeBy amount acc = acc {accountStake = accountStake acc + amount}
