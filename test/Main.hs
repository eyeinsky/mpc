module Main (main) where

import Hedgehog ((===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty

import Network.Socket qualified as N

import LocalPrelude
import DSL
import Interpreters
import Interpreters.Socket qualified as Socket
import Protocols qualified


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "MPC"
  [ Tasty.testGroup "Trivial"
    [ Tasty.testProperty "Publish" prop_publish
    ]
  , Tasty.testGroup "Arithmetic"
    [ Tasty.testProperty "Addition" prop_add
    ]

  , Tasty.withResource Socket.connectedPair (\(a, b, _) -> N.gracefulClose a 5000 >> N.gracefulClose b 5000) $ \aquireSockets ->
    Tasty.testGroup "TCP"
      [ Tasty.testProperty "Sent words are received" $ prop_tcpWords aquireSockets ]

  , Tasty.withResource Socket.mkLocalhostTcpCluster snd $ \aquireSockets ->
    Tasty.testGroup "Runners" $
      [ Tasty.testProperty "TCP" (prop_tcp $ fst <$> aquireSockets)
      , Tasty.testProperty "UnixDomainSocket" unitTest_unixDomainSocket
      ]
  ]

-- * Property tests

-- | Published shares are the same as the original argument.
prop_publish :: H.Property
prop_publish = H.property $ do
  arg <- genSmall
  (s1, s2, s3) <- liftIO $ do
    nodes <- nodeConfs dropLogs ioRandom
    runNodesF1 nodes dropLogs Protocols.publish arg
  arg === s1
  arg === s2
  arg === s3

-- | Addition is the same in regular and MPC.
prop_add :: H.Property
prop_add = H.property $ testProtocol2 (\a b -> return $ a + b) (+) genSmall

-- * Interpreter tests

unitTest_unixDomainSocket :: H.Property
unitTest_unixDomainSocket = unitTest $ do
  return ()

prop_tcp :: IO (Conf, Conf, Conf) -> H.Property
prop_tcp aquireIO = H.property $ do
  let add :: Word -> Word -> Program Word
      add a b = return $ a + b
  (n1, n2, n3) <- liftIO aquireIO
  arg1 <- genBounded
  arg2 <- genBounded
  res <- liftIO $ runNodesF2 [n1, n2, n3] dropLogs add arg1 arg2
  unshare res === arg1 + arg2

prop_tcpWords :: IO (N.Socket, N.Socket, N.PortNumber) -> H.Property
prop_tcpWords aquireIO = H.property $ do
  (write, read, _port) <- liftIO aquireIO
  n <- H.forAll $ Gen.integral $ Range.linear 0 100
  expected :: [Word] <- replicateM n genBounded
  liftIO $ mapM_ (Socket.sendWord write) expected
  got <- liftIO $ replicateM n (Socket.receiveWord read)
  let note = "sent " <> show expected
  -- liftIO $ putStrLn note
  H.footnote note
  expected === got

-- * Helpers

-- | Test 2-argument protocol.
testProtocol2
  :: ( Share a, Share b, Eq b, Show b)
  => (a -> a -> Program b)
  -> (a -> a -> b)
  -> (H.PropertyT IO a)
  -> H.PropertyT IO ()
testProtocol2 protocol oracle genArg = do
  arg1 <- genArg
  arg2 <- genArg
  nodes <- liftIO $ nodeConfs dropLogs ioRandom
  r <- liftIO $ runNodesF2 nodes dropLogs protocol arg1 arg2
  unshare r === oracle arg1 arg2

genBounded :: forall a . (Bounded a, Integral a, Show a) => H.PropertyT IO a
genBounded = H.forAll $ Gen.integral $ Range.linear minBound maxBound

genSmall :: H.PropertyT IO Word
genSmall = H.forAll $ Gen.integral $ Range.linear 0 10

unitTest :: H.PropertyT IO () -> H.Property
unitTest test = H.withTests 1 $ H.property test

runTest :: H.Property -> IO ()
runTest test = Tasty.defaultMain $ Tasty.testGroup "runTest" [ Tasty.testProperty "runTest" test ]

dropLogs :: String -> IO ()
dropLogs = const $ return ()
