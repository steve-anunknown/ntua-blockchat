module Node (Node(..))where

import qualified Data.Map as Map
import Network.Simple.TCP

data Node = Node {
    nodeId :: Int, -- Node ID as received from the bootstrap node (id = 0)
                   -- TODO: determine if nodeId field in data Node is actually necessary
    nodeIp :: HostName, -- Node IP address
    nodePort :: ServiceName, -- Node Port where it listens
    nodeNeighbours :: Map.Map HostName ServiceName -- Catalog of connected nodes and corresponding info
                                            -- Notice that each node only knows the IP and the Port
                                            -- of the other nodes.
}
-- host name and service name are just strings

-- TODO: figure out how the nodes are going to do the networking

