import Prelude as P

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import QCUtils
import Sound.Iteratee
import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24

-- ----------------------------------------
-- Word24 Properties

prop_addIdent a = a - a == 0
  where types = a :: Word24

prop_multIdent a = a * 1 == a
  where types = a :: Word24

prop_unsigned a = a >= 0
  where types = a :: Word24

prop_smaller a = (fromIntegral ((fromIntegral a) :: Word24) :: Word16) == a


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
