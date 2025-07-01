module Interpreters
  ( Conf(..), run
  ) where

import LocalPrelude
import Control.Monad.Free
import Control.Monad.Reader

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
