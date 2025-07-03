module NetworkSimpler where

import Prelude
import Data.Function
import Data.Functor
import Data.Word
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Exception
import Network.Socket as N
import System.Process
import Text.Read

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
--
-- | Endpoint is a host:port pair.

data Endpoint = Endpoint HostAddress PortNumber

instance Show Endpoint where
  show (Endpoint host port) = ipStr <> ":" <> show port
    where
      (a, b, c, d) = hostAddressToTuple host
      ipStr = intercalate "." $ map show [a, b, c, d]

instance Read Endpoint where
  readPrec = do
    a <- word '.'
    b <- word '.'
    c <- word '.'
    d <- word ':'
    portStr <- many get <* (look >>= \case x : _ -> when (isDigit x) $ fail ""; _ -> pure ())
    case readEither portStr of
      Right port -> pure $ Endpoint (tupleToHostAddress (a, b, c, d)) port
      Left err -> fail err

    where
      word :: Char -> ReadPrec Word8
      word f = do
        ds <- digits f
        case readEither ds of
          Right w -> return w
          Left msg -> fail $ "Can't parse as Word8: " <> ds <> ", error: " <> msg

      digits :: Char -> ReadPrec String
      digits f = do
        d <- get
        if | isDigit d -> (d :) <$> digits f
           | d == f -> pure []
           | otherwise -> fail $ "following character doesn't match expected " <> show f

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

-- * Constants

localhost :: HostAddress
localhost = 0x0100007f

-- * Measure

type Dest = ((Endpoint, Endpoint, String, String, String, String), String)

-- | Use ss to get bytes sent and received
statsByDst :: Endpoint -> IO (Maybe Dest)
statsByDst e = do
  let cp = shell $ "ss -itpn dst = " <> show e
  ssOut <- readCreateProcess cp ""
  let matches = parseSs ssOut
  return $ listToMaybe matches
  where
    parseSs :: String -> [Dest]
    parseSs str = lines str
      & tail
      & tails
      & map (take 2)
      & mapMaybe (\case [a, b] -> Just (socketInfo a b, a <> "\n" <> b)
                        _ -> Nothing)

    socketInfo a b = let
      ws = words a
      ps = words b
      in ( read $ ws !! 3 :: Endpoint -- from
         , read $ ws !! 4 :: Endpoint -- to
         , ws !! 5 -- name, pid, fd
         , getProp "bytes_sent:" ps
         , getProp "bytes_acked:" ps
         , getProp "bytes_received:" ps
         )

    getProp name ws =
      fromMaybe "" $ join $ find isJust $ map (stripPrefix name) ws
