{-# LANGUAGE NoImplicitPrelude #-}
module Convex.Muesli.LP.OnChain.OnChainUtils(
  integerToBS
) where

import           PlutusTx.Prelude (BuiltinByteString, Integer, consByteString,
                                   emptyByteString, negate, otherwise, quotient,
                                   remainder, ($), (+), (<), (<>), (==))

minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48

-- Convert from an integer to its text representation. Example: 123 => "123"
-- (Note: not compatible with bsToInteger as defined below, but that's ok)
{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString
