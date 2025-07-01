module LocalPrelude
  ( module LocalPrelude
  , module Export
  ) where

import Prelude as Export
import Control.Monad as Export
import Control.Monad.IO.Class as Export
import Text.Read as Export (readEither)
import System.Random -- as Export
import Control.Concurrent

sleep :: Int -> IO ()
sleep n = threadDelay $ n * 1_000_000

u :: a
u = undefined

todo :: String -> a
todo msg = error msg

ioRandom :: Random a => IO a
ioRandom = getStdRandom random
