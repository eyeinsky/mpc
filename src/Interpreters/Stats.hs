module Interpreters.Stats where

import LocalPrelude
import Foreign.Storable
import Control.Monad.Free
import DSL


data Stats = Stats
  { sent :: Int
  , received :: Int
  } deriving Show

countBytes :: Program a -> Stats
countBytes = interpret (Stats 0 0)

interpret :: Stats -> Program a -> Stats
interpret s p = case p of
  Free a -> case a of
    ShiftLeft v next -> interpret (add v s) $ next v
    ShiftRight v next -> interpret (add v s) $ next v
    Random next -> interpret s $ next 0
    Msg _msg next -> interpret s next
  Pure _v -> s
  where
    -- Use sizeOf from Foreign.Storable.sizeOf as it matches what we
    -- currently do. We don't actually use Storable to serialize the
    -- values though.
    add :: Word -> Stats -> Stats
    add v Stats { sent, received } = Stats
      { sent = sent + sizeOf v
      , received = received + sizeOf v
      }
