module Utils where

import Data.Bits ((.&.), shiftR, testBit)
import Data.Word (Word32)

word32ToFloat :: Word32 -> Float
word32ToFloat w = 
    let sign = if testBit w 31 then -1.0 else 1.0
        exponent = fromIntegral ((w `shiftR` 23) .&. 0xFF) :: Int
        mantissa = fromIntegral (w .&. 0x7FFFFF) :: Float
        normalizedMantissa = 1.0 + (mantissa / 8388608.0)
        realExponent = exponent - 127
    in sign * normalizedMantissa * (2.0 ** fromIntegral realExponent)