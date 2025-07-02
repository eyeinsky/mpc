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
import Interpreters.MVar qualified as MVar
import Interpreters.Socket qualified as Socket
import Protocols qualified


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "MPC"
  [ Tasty.testGroup "Protocols"
    [ Tasty.testProperty "Publish" prop_publish
    , Tasty.testProperty "Addition" prop_add
    , Tasty.testProperty "ifThenElse" prop_ifThenElse
    , Tasty.testProperty "ifThenElse2" prop_ifThenElse2
    ]

  , Tasty.withResource Socket.connectedPair (\(a, b, _) -> N.gracefulClose a 5000 >> N.gracefulClose b 5000) $ \aquireSockets ->
    Tasty.testGroup "TCP"
      [ Tasty.testProperty "Sent words are received" $ prop_tcpWords aquireSockets ]

  , Tasty.withResource Socket.mkLocalhostTcpCluster snd $ \aquireSockets ->
    Tasty.testGroup "Runners" $
      [ Tasty.testProperty "TCP" (prop_tcp $ fst <$> aquireSockets)
      -- , Tasty.testProperty "UnixDomainSocket" unitTest_unixDomainSocket
      ]
  ]

-- * Property tests

-- | Published shares are the same as the original argument.
prop_publish :: H.Property
prop_publish = H.property $ do
  arg <- genSmall
  s1 :| s2 :| s3 :| Nil <- runCluster =<< fmap Protocols.publish <$> share' arg
  arg === s1
  arg === s2
  arg === s3

-- | Addition is the same in regular and MPC.
prop_add :: H.Property
prop_add = H.property $ do
  arg1 <- genSmall
  arg2 <- genSmall
  r <- runCluster =<< zipWith (\a b -> return $ a + b) <$> share' arg1 <*> share' arg2
  unshare r === arg1 + arg2

-- * If-then-else

mkIfThenElse :: (Word -> Word -> Word -> Program Word) -> H.Property
mkIfThenElse protocol = H.property $ do
  bool' :: Bool <- H.forAll Gen.bool
  let bool = if bool' then 1 else 0 :: Word
  then_ <- genBounded
  else_ <- genBounded
  result <- runCluster =<< zipWith3 protocol <$> share' bool <*> share' then_ <*> share' else_
  (if bool' then then_ else else_) === unshare result

-- | If-then-else
prop_ifThenElse :: H.Property
prop_ifThenElse = mkIfThenElse Protocols.ifThenElse

-- | Another implementation
prop_ifThenElse2 :: H.Property
prop_ifThenElse2 = mkIfThenElse Protocols.ifThenElse2

-- * Interpreter tests

unitTest_unixDomainSocket :: H.Property
unitTest_unixDomainSocket = unitTest $ do
  return ()

prop_tcp :: IO (P3 Conf) -> H.Property
prop_tcp aquireIO = H.property $ do
  let add :: Word -> Word -> Program Word
      add a b = return $ a + b
  tcpNodes <- liftIO aquireIO
  arg1 <- genBounded
  arg2 <- genBounded
  res <- liftIO $ MVar.runNodes tcpNodes =<< zipWith add <$> share arg1 <*> share arg2
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

runCluster :: MonadIO m => P3 (Program Word) -> m (P3 Word)
runCluster programs = liftIO $ do
  nodes <- MVar.nodeConfs dropLogs ioRandom
  MVar.runNodes nodes programs

-- | Convenience function to produce shares in any MonadIO monad.
share' :: (MonadIO m, Share a) => a -> m (P3 a)
share' = liftIO . share

-- | Generate shares directly when initial values aren't required.
genShares :: forall a . (Bounded a, Integral a, Show a, Share a) => H.PropertyT IO (P3 a)
genShares = liftIO . share =<< genBounded

genBounded :: forall a . (Bounded a, Integral a, Show a) => H.PropertyT IO a
genBounded = H.forAll $ Gen.integral $ Range.linear minBound maxBound

genSmall :: H.PropertyT IO Word
genSmall = H.forAll $ Gen.integral $ Range.linear 0 10

unitTest :: H.PropertyT IO () -> H.Property
unitTest test = H.withTests 1 $ H.property test

runTest_ :: H.Property -> IO ()
runTest_ = runTest "-"

runTest :: String -> H.Property -> IO ()
runTest label test = let label' = "runTest: " <> label
  in Tasty.defaultMain $ Tasty.testGroup label' [ Tasty.testProperty label' test ]

dropLogs :: String -> IO ()
dropLogs = const $ return ()
