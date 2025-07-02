module LocalPrelude
  ( module LocalPrelude
  , module Export
  ) where

import Prelude as Export hiding (init, head, last, reverse, zip, zip3, zipWith, zipWith3)
import Control.Monad as Export
import Control.Monad.IO.Class as Export
import Text.Read as Export (readEither)
import System.Random -- as Export
import Control.Concurrent

import Data.List.Fixed.GADT as Export hiding ((++), todo, length)

sleep :: Int -> IO ()
sleep n = threadDelay $ n * 1_000_000

u :: a
u = undefined

todo :: String -> a
todo msg = error msg

ioRandom :: Random a => IO a
ioRandom = getStdRandom random
