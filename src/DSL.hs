module DSL where

import Prelude
import System.Random qualified as Random
import Control.Monad.Free


data MPC next
  = ShiftLeft Word (Word -> next)  -- from next
  | ShiftRight Word (Word -> next) -- from prev
  | Random (Word -> next)
  | Msg String next
  deriving Functor

type Program = Free MPC

shiftLeft :: Word -> Program Word
shiftLeft v = liftF (ShiftLeft v id)

shiftRight :: Word -> Program Word
shiftRight v = liftF (ShiftRight v id)

random :: Program Word
random = liftF (Random id)

msg :: String -> Program ()
msg str = liftF (Msg str ())

-- * Shares

type Shared a = (a, a, a)

class Share a where
  share :: a -> IO (a, a, a)
  unshare :: (a, a, a) -> a

instance Share Word where
  share v = do
    a <- Random.getStdRandom Random.random
    b <- Random.getStdRandom Random.random
    return (a, b, v - a - b)
  unshare (r1, r2, r3) = r1 + r2 + r3
