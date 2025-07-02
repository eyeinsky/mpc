module Interpreters.Socket where

import LocalPrelude
import DSL

import Data.List
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import NetworkSimpler qualified as NS
import System.Directory
import System.Random -- as Export

import Interpreters

-- * Unix domain sockets

-- | Create node to exchange data via Unix domain sockets.
socketConf :: IO Word -> FilePath -> FilePath -> FilePath -> FilePath -> IO Conf
socketConf getRandom receiveLeftP receiveRightP sendLeftP sendRightP = do
  -- create receiving sockets
  receiveLeftSocket <- NS.createUnixDomainSocket 1 receiveLeftP
  receiveRightSocket <- NS.createUnixDomainSocket 1 receiveRightP
  -- connect to sending sockets
  sendLeft <- mkSender sendLeftP
  sendRight <- mkSender sendRightP
  -- accept receiving sockets
  receiveLeft <- mkReceiver receiveLeftSocket
  receiveRight <- mkReceiver receiveRightSocket
  return $ Conf { receiveLeft, receiveRight, sendLeft, sendRight, getRandom, logger = todo "logger" }
  where
    -- | Connect to socket at @path@ and return function which sends a value on each call.
    mkSender :: FilePath -> IO (Word -> IO ())
    mkSender path = do
      waitPath path $ sleep 1
      s <- NS.connectUnixDomainSocket path
      return $ void . N.send s . BS8.pack . show

    -- | Create function that on each call retreives a value from the @socket@.
    mkReceiver :: N.Socket -> IO (IO Word)
    mkReceiver socket = do
      (socket',_) <- N.accept socket
      return $ receiveReadable socket'

-- * TCP

-- | Create node conf from pre-connected sockets to left and right.
connectedSocketConf :: IO Word -> (String -> IO ()) -> N.Socket -> N.Socket -> Conf
connectedSocketConf getRandom logger left right = Conf
  { receiveLeft = receiveWord left
  , receiveRight = receiveWord right
  , sendLeft = sendWord left
  , sendRight = sendWord right
  , getRandom
  , logger }

mkLocalhostTcpCluster :: IO (P3 Conf, IO ())
mkLocalhostTcpCluster = do
  let p12 = NS.Endpoint NS.localhost 30002
      p23 = NS.Endpoint NS.localhost 30003
      p31 = NS.Endpoint NS.localhost 30001

  -- listen on sockets
  l31' <- NS.createTCP' p31
  l12' <- NS.createTCP' p12
  l23' <- NS.createTCP' p23

  -- connect above created sockets
  c31 <- NS.connectTCP' p31
  c12 <- NS.connectTCP' p12
  c23 <- NS.connectTCP' p23

  -- accept these connections
  l31 <- fst <$> N.accept l31'
  l12 <- fst <$> N.accept l12'
  l23 <- fst <$> N.accept l23'

  let n1 = connectedSocketConf ioRandom putStrLn l31 c12
      n2 = connectedSocketConf ioRandom putStrLn l12 c23
      n3 = connectedSocketConf ioRandom putStrLn l23 c31
      close = forM_ -- forConcurrently_
        [ c31, c12, c23
        , l31, l12, l23
        ] $ \s -> do
        -- N.close' s
        N.gracefulClose s 5000

  return (n1 :| n2 :| n3 :| Nil, close)

-- | Receive from accepted socket.
receiveReadable :: N.Socket -> IO Word
receiveReadable socket' = do
  str <- BS8.unpack <$> N.recv socket' 4096
  case readEither str of
    Right r -> return r
    Left err -> fail err

waitPath :: FilePath -> IO () -> IO ()
waitPath path action = loop
  where
    loop = do
      yes <- doesFileExist path
      if yes then return () else action >> loop

-- * Word

receiveWord :: N.Socket -> IO Word
receiveWord socket' = bsToWord <$> N.recv socket' 8
  where
    bsToWord :: BS.ByteString -> Word
    bsToWord = foldl' step 0 . BS.unpack
      where
        step acc byte = shiftL acc 8 .|. fromIntegral byte
        -- TODO: figure out how to remove Num constraint

sendWord :: N.Socket -> Word -> IO ()
sendWord s w = void $ N.send s $ BS.pack $ wordToWord8s w

wordToWord8s :: Word -> [Word8]
wordToWord8s w = go w (8::Int) []
  where
    go w' n acc = case n of
      0 -> acc
      _ -> go (shiftR w' 8) (n - 1) (fromIntegral w' : acc)

connectedPair :: IO (N.Socket, N.Socket, N.PortNumber)
connectedPair = do
  port <- fmap fromIntegral $ getStdRandom $ randomR @Word32 (32768, 60999)
  let e = NS.Endpoint NS.localhost port
  l <- NS.createTCP' e
  s <- NS.connectTCP' e
  l' <- fst <$> N.accept l
  return (l', s, port)
