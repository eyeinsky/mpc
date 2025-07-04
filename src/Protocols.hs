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

-- | Branch on boolean represented as 0 or 1. Multiplying by it
-- exactly preserves either then_ or else_ values.
ifThenElse :: Word -> Word -> Word -> Program Word
ifThenElse bool then_ else_ = do
  res <- multiply bool (then_ - else_)
  return (res + else_)

ifThenElse2 :: Word -> Word -> Word -> Program Word
ifThenElse2 bool then_ else_ = do
  a <- bool `multiply` then_
  b <- (1 - bool) `multiply` else_
  return $ a + b

-- | Result is zero if v is zero.
zeroTest :: Word -> Program Word
zeroTest v = do
  r <- random
  multiply v r

-- | Result is zero if args are equal.
equal :: Word -> Word -> Program Word
equal a b = zeroTest (a - b)
