module Account
  ( Account (..),
    emptyAccount,
    availableBalance,
  )
where


data Account = Account
  { 
    accountBalance :: Double, -- the balance of the account.
    accountNonce :: Int,
    -- a field that is incremented with every
    -- outgoing transaction, in order to guard
    -- against replay attacks.
    accountStake :: Double 
    -- the amount of stake that the account has
    -- for the PoS protocol.
  } deriving (Show, Eq)

emptyAccount :: Account
emptyAccount = Account 0 0 0

availableBalance :: Account -> Double
availableBalance Account {accountBalance = bal, accountStake = st} = bal - bal * st
