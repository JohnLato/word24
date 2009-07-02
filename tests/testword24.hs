import Prelude as P

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck hiding ((.&.))

import QCUtils
import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24
import Data.Bits
import GHC.Real

-- ----------------------------------------
-- Word24 Properties

prop_addIdent a = a - a == 0
  where types = a :: Word24

prop_multIdent a = a * 1 == a
  where types = a :: Word24

prop_unsigned a = a >= 0
  where types = a :: Word24

prop_smaller a = (fromIntegral ((fromIntegral a) :: Word24) :: Word16) == a

prop_show a = show a == show (fromIntegral a :: Word24)
  where types = a :: Word16

prop_adder a b = (a < ub) && (b < ub) ==>
                 fromIntegral (a + b) ==
                 ((fromIntegral a + fromIntegral b) :: Word24)
  where types = (a :: Word16, b :: Word16)
        ub :: Word16
        ub = maxBound `div` 2

prop_negate a = a == negate (negate a)
  where types = a :: Word24

prop_abs a = a == abs a
  where types = a :: Word24

prop_signum a = if a == 0 then signum a == a else signum a == 1
  where types = a :: Word24

prop_real a = let r = toRational a in numerator r == fromIntegral a
  where types = a :: Word24

prop_enum1 a = a < maxBound ==> succ a == a + 1
  where types = a :: Word24

prop_enum2 a = a > minBound ==> pred a == a - 1
  where types = a :: Word24

prop_enum3 a = let a' = abs a in
               toEnum a' == fromIntegral a'
  where types = a :: Int

prop_enum4 a = a < maxBound ==> take 2 (enumFrom a) == [a, a+1]
  where types = a :: Word24

prop_enum5 a b = let b' = fromIntegral b in
                 enumFromTo a (a + b') ==
                 map fromIntegral (enumFromTo (fromIntegral a :: Integer)
                                             (fromIntegral (a + b') :: Integer))
  where types = (a :: Word24, b :: Word8)

prop_enum6 a b = take 2 (enumFromThen a b) == [a,b]
  where types = (a :: Word24, b :: Word24)

prop_quot a b =
  quot a b == fromIntegral (quot (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_rem a b = 
  rem a b == fromIntegral (rem (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_div a b = 
  div a b == fromIntegral (div (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_mod a b = 
  mod a b == fromIntegral (mod (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_quotrem a b = let (j, k) = quotRem a b in
  a == (b * j) + k
  where types = (a :: Word24, b :: Word24)

prop_and a b = (a .&. b) == fromIntegral ( (fromIntegral a :: Word24) .&. (fromIntegral b :: Word24))
  where types = (a :: Word16, b :: Word16)

prop_or a b = (a .|. b) == fromIntegral ( (fromIntegral a :: Word24) .|. (fromIntegral b :: Word24))
  where types = (a :: Word16, b :: Word16)

prop_xor a b = (a `xor` b) == fromIntegral ( (fromIntegral a :: Word24) `xor` (fromIntegral b :: Word24))
  where types = (a :: Word16, b :: Word16)

prop_xor_ident a b = (a `xor` b) `xor` b == a
  where types = (a :: Word24, b :: Word24)

prop_shiftL a = a `shiftL` 1 == a * 2
  where types = a :: Word24

prop_shiftL_ident a = a `shiftL` 0 == a
  where types = a :: Word24

prop_rotate a b = (a `rotate` b) `rotate` (negate b) == a
  where types = (a :: Word24, b :: Int)

prop_comp a = complement (complement a) == a
  where types = a :: Word24


-- ----------------------------------------
-- Int24 Properties

prop_addIdentI a = a - a == 0
  where types = a :: Int24

prop_multIdentI a = a * 1 == a
  where types = a :: Int24

prop_smallerI a = (fromIntegral ((fromIntegral a) :: Int24) :: Int16) == a


-- ----------------------------------------
-- tests
tests = [
  testGroup "Word24" [
    testProperty "add. identity" prop_addIdent
    ,testProperty "mult. identity" prop_multIdent
    ,testProperty "unsigned" prop_unsigned
    ,testProperty "Word16/Word24 conversion" prop_smaller
    ,testProperty "Show" prop_show
    ,testProperty "addition" prop_adder
    ,testProperty "negate identity" prop_negate
    ,testProperty "absolute value" prop_abs
    ,testProperty "signum" prop_signum
    ,testProperty "Real identity" prop_real
    ,testProperty "quot" prop_quot
    ,testProperty "rem" prop_rem
    ,testProperty "div" prop_div
    ,testProperty "mod" prop_mod
    ,testProperty "quotRem" prop_quotrem
    ]
  ,testGroup "Word24 Enum instance" [
    testProperty "enum succ" prop_enum1
    ,testProperty "enum pred" prop_enum2
    ,testProperty "toEnum" prop_enum3
    ,testProperty "enumFrom " prop_enum4
    ,testProperty "enumFromTo" prop_enum5
    ,testProperty "enumFromThen" prop_enum6
    ]
  ,testGroup "Word24 binary instance" [
    testProperty "binary and" prop_and
    ,testProperty "binary or" prop_or
    ,testProperty "binary xor" prop_xor
    ,testProperty "binary xor identity" prop_xor_ident
    ,testProperty "binary shiftL" prop_shiftL
    ,testProperty "binary shiftL identity" prop_shiftL_ident
    ,testProperty "binary rotate" prop_rotate
    ,testProperty "binary complement" prop_comp
    ]
  ,testGroup "Int24" [
    testProperty "add. identity" prop_addIdentI
    ,testProperty "mult. identity" prop_multIdentI
    ,testProperty "Int16/Int24 conversion" prop_smallerI
    ]
  ]

-- ----------------------------------------
-- entry point

main = defaultMain tests
