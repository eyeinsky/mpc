module Protocols where

import LocalPrelude
import DSL

reshare :: Word -> Program Word
reshare v = do
  r <- random
  r' <- shiftLeft r
  return $ v + r - r'

-- | Protocol to make the argument value public
publish :: Word -> Program Word
publish v = do
  vn <- shiftLeft v
  vp <- shiftRight v
  return $ unshare $ v :| vn :| vp :| Nil

multiply :: Word -> Word -> Program Word
multiply a b = do
  x <- reshare a
  y <- reshare b
  x' <- shiftRight x
  y' <- shiftLeft y
  return $ (x * y) + (x' * y) + (x' * y')
