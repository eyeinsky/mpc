module Interpreters
  ( Conf(..), run
  , runNodes, runNodesF1, runNodesF2
  , nodeConfs
  ) where

import LocalPrelude
import Control.Monad.Free
import Control.Monad.Reader
import Control.Concurrent qualified as IO

import DSL


data Conf = Conf
  { getRandom    :: IO Word
  , logger       :: String -> IO ()

  , receiveRight :: IO Word
  , receiveLeft  :: IO Word
  , sendRight    :: Word -> IO ()
  , sendLeft     :: Word -> IO ()
  }

-- | Run protocol to its output share (the analyst would collect these
-- and unshare them into a final value).
run :: Program a -> Conf -> IO a
run protocol env = runReaderT (interpret protocol) env

-- | Interpret protocol in terms of IONode, i.e "run it".
interpret :: Program a -> ReaderT Conf IO a
interpret p = case p of
  Free a -> case a of
    ShiftLeft v next -> do
      asks sendLeft >>= \f -> liftIO $ f v
      v' <- liftIO =<< asks receiveRight
      logM $ "sent " <> show v <> ", received " <> show v' <> ", with next"
      interpret $ next v'
    ShiftRight v next -> do
      asks sendRight >>= \f -> liftIO $ f v
      v' <- liftIO =<< asks receiveLeft
      logM $ "sent " <> show v <> ", received " <> show v' <> ", with prev"
      interpret $ next v'
    Random next -> do
      v <- liftIO =<< asks getRandom
      logM $ "generated random " <> show v
      interpret $ next v
    Msg msg' next -> do
      logM msg'
      interpret next

  Pure v -> return v

  where
    logM :: String -> ReaderT Conf IO ()
    logM msg' = asks logger >>= \f -> liftIO $ f msg'

-- * In-process nodes

type Channels = (IO.MVar Word, IO.MVar Word)

nodeConfs :: (String -> IO ()) -> IO Word -> IO [Conf]
nodeConfs logger getRandom = do
  [m1, m2, m3] <- replicateM 3 $ (,) <$> IO.newEmptyMVar <*> IO.newEmptyMVar
  return
    [ mkConf m1 m3 m2
    , mkConf m2 m1 m3
    , mkConf m3 m2 m1
    ]
  where
    mkConf :: Channels -> Channels -> Channels -> Conf
    mkConf (fp, fn) (_, pfn) (nfp, _) = Conf
      { sendRight    = IO.putMVar nfp
      , sendLeft     = IO.putMVar pfn
      , receiveRight = IO.takeMVar fn
      , receiveLeft  = IO.takeMVar fp
      , logger
      , getRandom }

runNodesF2
  :: forall a b c . (Share a, Share b, Share c)
  => [Conf]
  -> (String -> IO ())
  -> (a -> b -> Program c) -> a -> b -> IO (c, c, c)
runNodesF2 nodes logger protocol arg1 arg2 = do
  log_ "create node-specific protocols"
  programs <- do
    (a1, a2, a3) <- share arg1
    (b1, b2, b3) <- share arg2
    return [protocol a1 b1, protocol a2 b2, protocol a3 b3]
  runNodes nodes programs log_
  where
    log_ msg' = logger $ "[main] " <> msg'

runNodesF1
  :: forall a b . (Share a, Share b)
  => [Conf]
  -> (String -> IO ()) -> (a -> Program b) -> a -> IO (b, b, b)
runNodesF1 nodes logger protocol arg = do
  log_ "create node-specific protocols"
  programs :: [Program b] <- do
    (a1, a2, a3) <- share arg
    return [protocol a1, protocol a2, protocol a3]
  runNodes nodes programs log_
  where
    log_ msg' = logger $ "[main] " <> msg'

runNodes :: forall b . Share b => [Conf] -> [Program b] -> (String -> IO ()) -> IO (b, b, b)
runNodes nodes programs log_ = do
  log_ "run nodes"
  outboxes <- forM nodes $ const $ IO.newEmptyMVar
  forM_ (zip3 nodes programs outboxes) $ \(nodeConf, program, outbox) ->
    IO.forkIO $ void $ run program nodeConf >>= IO.putMVar outbox

  log_ "collect results, unshare and return"
  [o1, o2, o3] <- mapM IO.takeMVar outboxes -- Await on all outboxes to be filled
  return (o1, o2, o3) -- Put node outputs back together again.
