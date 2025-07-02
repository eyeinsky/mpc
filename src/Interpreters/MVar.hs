module Interpreters.MVar where

import LocalPrelude
import Control.Concurrent qualified as IO

import DSL
import Interpreters


type Channels = (IO.MVar Word, IO.MVar Word)

nodeConfs :: (String -> IO ()) -> IO Word -> IO (P3 Conf)
nodeConfs logger getRandom = do
  m1 <- (,) <$> IO.newEmptyMVar <*> IO.newEmptyMVar
  m2 <- (,) <$> IO.newEmptyMVar <*> IO.newEmptyMVar
  m3 <- (,) <$> IO.newEmptyMVar <*> IO.newEmptyMVar
  return
     $ mkConf m1 m3 m2
    :| mkConf m2 m1 m3
    :| mkConf m3 m2 m1
    :| Nil

  where
    mkConf :: Channels -> Channels -> Channels -> Conf
    mkConf (fp, fn) (_, pfn) (nfp, _) = Conf
      { sendRight    = IO.putMVar nfp
      , sendLeft     = IO.putMVar pfn
      , receiveRight = IO.takeMVar fn
      , receiveLeft  = IO.takeMVar fp
      , logger
      , getRandom }

runNodes :: forall b . Share b => P3 Conf -> P3 (Program b) -> IO (P3 b)
runNodes nodes programs = do
  outboxes :: P3 (IO.MVar b) <- forM nodes $ const $ IO.newEmptyMVar
  let fork nodeConf program outbox = IO.forkIO $ void $ run program nodeConf >>= IO.putMVar outbox
  sequence_ $ zipWith3 fork nodes programs outboxes
  mapM IO.takeMVar outboxes -- Await on all outboxes to be filled
