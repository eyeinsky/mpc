module NetworkSimpler where

import Prelude
import Data.Functor
import Control.Exception
import Network.Socket as N


-- | Listen on type at address, with maxCount simultaneous connections.
createPrim :: SocketType -> Family -> SockAddr -> IO Socket
createPrim type_ family address = do
  s <- socket family type_ 0
  bind s address
  return s

startListen :: Int -> Socket -> IO Socket
startListen maxCount socket' = listen socket' maxCount $> socket'

connectPrim :: SocketType -> Family -> SockAddr -> IO Socket
connectPrim type_ family address = do
  s <- socket family type_ 0
  N.connect s address
  return s

-- tcp ipv4 is:  family = AF_INET; address = SockAddrInet port ip
-- tcp ipv6 is:  family = AF_INET6; address = SockAddrInet6 PortNumber FlowInfo HostAddress6 ScopeID
-- uds is:  family = AF_UNIX; address = SockAddrUnix path

-- * Unix domain socket

createUnixDomainSocket :: Int -> FilePath -> IO Socket
createUnixDomainSocket maxCount socketPath = startListen maxCount =<< createPrim Stream AF_UNIX (SockAddrUnix socketPath)

connectUnixDomainSocket :: FilePath -> IO Socket
connectUnixDomainSocket p = connectPrim Stream AF_UNIX (SockAddrUnix p)

-- * IP

data Endpoint = Endpoint HostAddress PortNumber
  deriving Show

-- ** TCP

createTCP :: HostAddress -> PortNumber -> Int -> IO Socket
createTCP ip port maxCount = startListen maxCount =<< createPrim Stream AF_INET (SockAddrInet port ip)

createTCP' :: Endpoint -> IO Socket
createTCP' (Endpoint h p) = createTCP h p 1

connectTCP :: HostAddress -> PortNumber -> IO Socket
connectTCP ip port = connectPrim Stream AF_INET (SockAddrInet port ip)

connectTCP' :: Endpoint -> IO Socket
connectTCP' (Endpoint h p) = connectTCP h p

connectTcpLoop :: Endpoint -> (IOException -> IO ()) -> IO Socket
connectTcpLoop e action = loop
  where
    loop = try (connectTCP' e) >>= \case
      Right c' -> return c'
      Left e' -> action e' >> loop

-- ** UDP

createUDP :: HostAddress -> PortNumber -> Int -> IO Socket
createUDP ip port maxCount = startListen maxCount =<< createPrim Datagram AF_INET (SockAddrInet port ip)

connectUDP :: HostAddress -> PortNumber -> IO Socket
connectUDP ip port = connectPrim Datagram AF_INET (SockAddrInet port ip)

-- * predefined

localhost :: HostAddress
localhost = 0x0100007f
