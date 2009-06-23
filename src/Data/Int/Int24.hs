{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Int.Int24
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see src/Data/LICENSE
-- 
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The 3 bit integral datatypes, 'Int24'.
--
-----------------------------------------------------------------------------

-- #hide
module Data.Int.Int24 (
    Int24(..),
    narrow24Int#,
    ) where

import Data.Word.Word24

import Data.Bits
import Foreign.Storable

import GHC.Base
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Read
import GHC.Arr
import GHC.Word hiding (uncheckedShiftL64#, uncheckedShiftRL64#)
import GHC.Int
import GHC.Show
import GHC.Ptr

------------------------------------------------------------------------
-- type Int24
------------------------------------------------------------------------

-- Int24 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

data Int24 = I24# Int# deriving (Eq, Ord)
-- ^ 24-bit signed integer type

-- the narrowings are primops in GHC; I don't have that luxury.
narrow24Int# :: Int# -> Int#
narrow24Int# x# = if int2Word# x# `gtWord#` int2Word# 0x007FFFFF#
  then word2Int# ( int2Word# x# `or#` int2Word# 0xFF800000# )
  else x#

instance Show Int24 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Int24 where
    (I24# x#) + (I24# y#)  = I24# (narrow24Int# (x# +# y#))
    (I24# x#) - (I24# y#)  = I24# (narrow24Int# (x# -# y#))
    (I24# x#) * (I24# y#)  = I24# (narrow24Int# (x# *# y#))
    negate (I24# x#)       = I24# (narrow24Int# (negateInt# x#))
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger i          = I24# (narrow24Int# (toInt# i))

instance Real Int24 where
    toRational x = toInteger x % 1

instance Enum Int24 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int24"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int24"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int24) && i <= fromIntegral (maxBound::Int24)
                        = I24# i#
        | otherwise     = toEnumError "Int24" i (minBound::Int24, maxBound::Int24)
    fromEnum (I24# x#)  = I# x#
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Int24 where
    quot    x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `quotInt#` y#))
    rem     x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `remInt#` y#))
    div     x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `divInt#` y#))
    mod     x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `modInt#` y#))
    quotRem x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = (I24# (narrow24Int# (x# `quotInt#` y#)),
                                        I24# (narrow24Int# (x# `remInt#` y#)))
    divMod  x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = (I24# (narrow24Int# (x# `divInt#` y#)),
                                        I24# (narrow24Int# (x# `modInt#` y#)))
    toInteger (I24# x#)              = smallInteger x#

instance Bounded Int24 where
    minBound = -0x800000
    maxBound =  0x7FFFFF

instance Ix Int24 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

instance Read Int24 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Int24 where
    {-# INLINE shift #-}

    (I24# x#) .&.   (I24# y#)  = I24# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I24# x#) .|.   (I24# y#)  = I24# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I24# x#) `xor` (I24# y#)  = I24# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I24# x#)       = I24# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))
    (I24# x#) `shift` (I# i#)
        | i# >=# 0#            = I24# (narrow24Int# (x# `iShiftL#` i#))
        | otherwise            = I24# (x# `iShiftRA#` negateInt# i#)
    (I24# x#) `rotate` (I# i#)
        | i'# ==# 0# 
        = I24# x#
        | otherwise
        = I24# (narrow24Int# (word2Int# ((int2Word# x'# `uncheckedShiftL#` i'#) `or#`
                                         (int2Word# x'# `uncheckedShiftRL#` (24# -# i'#)))))
        where
        x'# = narrow24Int# x#
        i'# = word2Int# (int2Word# i# `and#` int2Word# 15#)
    bitSize  _                 = 24
    isSigned _                 = True

    {-# INLINE shiftR #-}
    -- same as the default definition, but we want it inlined (#2376)
    x `shiftR`  i = x `shift`  (-i)

{-# RULES
"fromIntegral/Word8->Int24"   fromIntegral = \(W8# x#) -> I24# (word2Int# x#)
"fromIntegral/Word16->Int24"  fromIntegral = \(W16# x#) -> I24# (word2Int# x#)
"fromIntegral/Int8->Int24"    fromIntegral = \(I8# x#) -> I24# x#
"fromIntegral/Int16->Int24"   fromIntegral = \(I16# x#) -> I24# x#
"fromIntegral/Int24->Int24"   fromIntegral = id :: Int24 -> Int24
"fromIntegral/a->Int24"       fromIntegral = \x -> case fromIntegral x of I# x# -> I24# (narrow24Int# x#)
"fromIntegral/Int24->a"       fromIntegral = \(I24# x#) -> fromIntegral (I# x#)
  #-}

instance Storable Int24 where
  sizeOf _ = 3
  alignment _ = 3
  peek p = fmap fromIntegral $ peek ((castPtr p) :: Ptr Word24)
  poke p v = poke (castPtr p :: Ptr Word24) (fromIntegral v)
