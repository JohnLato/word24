{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.Word.Word24
-- License     : see  src/Data/LICENSE
-- Stability   : experimental
-- Portability : non-portable (GHC Extensions)

-- Provide a three-byte unsigned integral type: 'Word24', analagous to Word8,
-- Word16, etc.
-- 

-- #hide
module Data.Word.Word24 (
  Word24(..)
  )

where

import Data.Bits
import Foreign.Storable

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Show
import GHC.Word
import GHC.Ptr

-- Word24 is represented in the same way as Word.  Operations may assume and
-- must ensure that it holds only values in its logical range.

data Word24 = W24# Word# deriving (Eq, Ord)
-- ^ 24-bit unsigned integer type

-- the narrowings are represented as primops in GHC.  I don't have that
-- luxury...

narrow24Word# :: Word# -> Word#
narrow24Word# x# = x# `and#` (int2Word# 0xFFFFFF#)

instance Show Word24 where
  showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Word24 where
  (W24# x#) + (W24# y#) = W24# (narrow24Word# (x# `plusWord#` y#))
  (W24# x#) - (W24# y#) = W24# (narrow24Word# (x# `minusWord#` y#))
  (W24# x#) * (W24# y#) = W24# (narrow24Word# (x# `timesWord#` y#))
  negate (W24# x#)      = W24# (narrow24Word# (int2Word# (negateInt# (word2Int# x#))))
  abs x                 = x
  signum 0              = 0
  signum _              = 1
  fromInteger i         = W24# (narrow24Word# (integerToWord i))

instance Real Word24 where
  toRational x = toInteger x % 1

instance Enum Word24 where
  succ x
    | x /= maxBound  = x + 1
    | otherwise      = succError "Word24"
  pred x
    | x /= minBound  = x - 1
    | otherwise      = predError "Word24"
  toEnum i@(I# i#)
    | i >= 0 && i <= fromIntegral (maxBound :: Word24)
                     = W24# (int2Word# i#)
    | otherwise      = toEnumError "Word24" i (minBound::Word24, maxBound::Word24)
  fromEnum (W24# x#) = I# (word2Int# x#)
  enumFrom           = boundedEnumFrom
  enumFromThen       = boundedEnumFromThen

instance Integral Word24 where
  quot (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `quotWord#` y#)
    | otherwise              = divZeroError
  rem (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `remWord#` y#)
    | otherwise              = divZeroError
  div (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `quotWord#` y#)
    | otherwise              = divZeroError
  mod (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `remWord#` y#)
    | otherwise              = divZeroError
  quotRem (W24# x#) y@(W24# y#)
    | y /= 0                 = (W24# (x# `quotWord#` y#), W24# (x# `remWord#` y#))
    | otherwise              = divZeroError
  divMod (W24# x#) y@(W24# y#)
    | y /= 0                 = (W24# (x# `quotWord#` y#), W24# (x# `remWord#` y#))
    | otherwise              = divZeroError
  toInteger (W24# x#)        = smallInteger (word2Int# x#)

instance Bounded Word24 where
  minBound = 0
  maxBound = 0xFFFFFF

instance Ix Word24 where
  range (m,n)         = [m..n]
  unsafeIndex (m,_) i = fromIntegral (i - m)
  inRange (m,n) i     = m <= i && i <= n

instance Read Word24 where
  readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Word24 where
  {-# INLINE shift #-}

  (W24# x#) .&.   (W24# y#) = W24# (x# `and#` y#)
  (W24# x#) .|.   (W24# y#) = W24# (x# `or#` y#)
  (W24# x#) `xor` (W24# y#) = W24# (x# `xor#` y#)
  complement (W24# x#)      = W24# (x# `xor#` mb#) where W24# mb# = maxBound
  (W24# x#) `shift` (I# i#)
    | i# >=# 0#             = W24# (narrow24Word# (x# `shiftL#` i#))
    | otherwise             = W24# (x# `shiftRL#` negateInt# i#)
  (W24# x#) `rotate` (I# i#)
    | i'# ==# 0# = W24# x#
    | otherwise  = W24# (narrow24Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                        (x# `uncheckedShiftRL#` (24# -# i'#))))
    where
    i'# = word2Int# (int2Word# i# `and#` int2Word# 23#)
  bitSize _                 = 24
  isSigned _                = False

  {-# INLINE shiftR #-}
  x `shiftR` i = x `shift` (-i)

{-# RULES
"fromIntegral/Word8->Word24"    fromIntegral = \(W8# x#) -> W24# x#
"fromIntegral/Word16->Word24"   fromIntegral = \(W16# x#) -> W24# x#
"fromIntegral/Word24->Word24"   fromIntegral = id :: Word24 -> Word24
"fromIntegral/Word24->Integer"  fromIntegral = toInteger :: Word24 -> Integer
"fromIntegral/a->Word24"        fromIntegral = \x -> case fromIntegral x of W# x# -> W24# (narrow24Word# x#)
"fromIntegral/Word24->a"        fromIntegral = \(W24# x#) -> fromIntegral (W# x#)
  #-}

readWord24OffPtr :: Ptr Word24 -> IO Word24
readWord24OffPtr p = do
  let p' = (castPtr p) :: Ptr Word8
  w1 <- peekElemOff p' 0
  w2 <- peekElemOff p' 1
  w3 <- peekElemOff p' 2
  let w1' = (fromIntegral :: (Word8 -> Word24)) w1
      w2' = (fromIntegral :: (Word8 -> Word24)) w2
      w3' = (fromIntegral :: (Word8 -> Word24)) w3
      w = w1' .|. (w2' `shiftL` 8) .|. (w3' `shiftL` 16)
  return $ fromIntegral w

writeWord24ToPtr :: Ptr Word24 -> Word24 -> IO ()
writeWord24ToPtr p v = do
    let w1 = fromIntegral (v .&. 0x0000FF) :: Word8
        w2 = (fromIntegral ((v .&. 0x00FF00) `shiftR` 8)) :: Word8
        w3 = (fromIntegral ((v .&. 0xFF0000) `shiftR` 16)) :: Word8
    pokeByteOff p 0 w1
    pokeByteOff p 1 w2
    pokeByteOff p 2 w3

instance Storable Word24 where
  sizeOf _    = 3
  alignment _ = 3
  peek p      = readWord24OffPtr p
  poke p v    = writeWord24ToPtr p v

