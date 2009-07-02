import Prelude as P

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import QCUtils
import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24
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

prop_enum3 a = a >= 0 && a < fromIntegral (maxBound :: Word24) ==>
               toEnum a == fromIntegral a
  where types = a :: Int

prop_enum4 a = a < maxBound ==> take 2 (enumFrom a) == [a, a+1]
  where types = a :: Word24

prop_enum5 a b = let a' = min a b
                     b' = max a b in
                 b' - a' <= 200000 ==> enumFromTo a' b' ==
                 map fromIntegral (enumFromTo (fromIntegral a' :: Integer)
                                              (fromIntegral b' :: Integer))
  where types = (a :: Word24, b :: Word24)

prop_enum6 a b = fromIntegral a < (maxBound :: Word24) && b < 10 ==>
  take 5 (enumFromThen a b) == map fromIntegral (take 5 (enumFromThen (fromIntegral a :: Word24) (fromIntegral b :: Word24)))
  where types = (a :: Word32, b :: Word32)

prop_quot a b = a <= fromIntegral (maxBound :: Word24) && b <= fromIntegral (maxBound :: Word24) ==>
  quot a b == fromIntegral (quot (fromIntegral a :: Word24) (fromIntegral b :: Word24))
  where types = a :: Word32

prop_rem a b = a <= fromIntegral (maxBound :: Word24) && b <= fromIntegral (maxBound :: Word24) ==>
  rem a b == fromIntegral (rem (fromIntegral a :: Word24) (fromIntegral b :: Word24))
  where types = a :: Word32

prop_div a b = a <= fromIntegral (maxBound :: Word24) && b <= fromIntegral (maxBound :: Word24) ==>
  div a b == fromIntegral (div (fromIntegral a :: Word24) (fromIntegral b :: Word24))
  where types = a :: Word32


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
    ,testProperty "enum succ" prop_enum1
    ,testProperty "enum pred" prop_enum2
    ,testProperty "toEnum" prop_enum3
    ,testProperty "enumFrom " prop_enum4
    ,testProperty "enumFromTo" prop_enum5
    ,testProperty "enumFromThen" prop_enum6
    ,testProperty "quot" prop_quot
    ,testProperty "rem" prop_rem
    ,testProperty "div" prop_div
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
