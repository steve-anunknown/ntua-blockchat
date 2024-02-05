module Main (main) where

import Lib
import System.Environment
import System.Exit

main = getArgs >>= parseArgs >>= startDoingStuff

startDoingStuff :: [String] -> IO a
startDoingStuff [num, host, port] = bootstrapNode (nodes, host, port)
    -- HostName = String and ServiceName = String so all ok
    where nodes = stringToInt 10 num
startDoingStuff [host, port] = ordinaryNode (host, port)

-- TODO: implement the logic for bootstrapNode
bootstrapNode (Int, HostName, ServiceName) :: IO a
bootstrapNode (nodes, host, port) = error "Not implemented"
-- bootstrapNode (nodes, host, port) = do
--     (bootstrapPubKey, boostrapPrivKey)<- generateWallet 2048
--     currTime <- getUnixTime

-- TODO: implement the logic for ordinaryNode
ordinaryNode (HostName, ServiceName) :: IO a
ordinaryNode (host, port) = error "Not implemented"

parseArgs :: [String] -> IO [String]
parseArgs ("--node" : restArgs) = help restArgs
    where help :: [String] -> IO [String]
          help ("--ip" : host : "--port" : port) = return [host, port]
          help _ = usage >> exit
parseArgs ("--boostrap" : restArgs) = help restArgs
    where help :: [String] -> IO [String]
          help ("--nodes" : num : "--ip" : host : "--port" : port) = return [num, host, port]
          help _ = usage >> exit
parseArgs _ = usage >> exit

usage :: IO ()
usage = putStrLn "Usage: main --node --ip <ip address> --port <port>\n
                  or     main --bootstrap --nodes <num nodes> --ip <ip address> --port <port>"

exit :: IO a
exit = exitWith ExitSuccess
