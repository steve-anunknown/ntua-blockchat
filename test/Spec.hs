-- TODO: Add tests for backend functions here
-- and fix the imports

import Data.ByteString.Char8 (pack)
import Backend (generateWallet, createTransaction)

main :: IO ()
main = testCreateTransaction



testCreateTransaction :: IO ()
testCreateTransaction = do
    -- Generate two wallets
    (pubKey1, _) <- generateWallet 256
    (pubKey2, _) <- generateWallet 256

    -- Create a transaction
    let transaction = createTransaction pubKey1 pubKey2 (Coins 10) 10 (pack "1234567890")

    -- Now you can test properties of the transaction
    -- For example, test that the 'from' field is equal to pubKey1
    if senderAddress transaction == pubKey1
        then putStrLn "Test passed!"
        else putStrLn "Test failed!"
