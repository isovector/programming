{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Monoids where

import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import GHC.Generics
import Data.Monoid
import Control.Comonad
import Test.QuickCheck.Checkers
import Checkers
import Test.QuickCheck.Classes
import Test.Hspec

data Average a = Average
  { numerator :: a
  , denominator :: Int
  }
  deriving stock Functor

  deriving stock Generic
  deriving stock Show
  deriving stock Eq


instance Comonad Average where
  extract (Average n _) = n
  extend f avg@(Average _ d) = Average (f avg) d


instance Num a => Semigroup (Average a) where
  Average n1 d1 <> Average n2 d2 = Average (n1 + n2) (d1 + d2)

instance Num a => Monoid (Average a) where
  mempty = Average 0 0

average :: a -> Average a
average a = Average a 1

getAverage :: Fractional a => Average a -> a
getAverage (Average _ 0) = 0
getAverage (Average n d) = n / fromIntegral d

--------------------------------------------------------------------------------

deriving via GenericArbitrary (Average (Sum Int)) instance Arbitrary (Average (Sum Int))
deriving via GenericArbitrary (Average Int) instance Arbitrary (Average Int)
deriving via GenericArbitrary (Average (Int -> Int)) instance Arbitrary (Average (Int -> Int))

deriving anyclass instance CoArbitrary (Average Int)


instance (Show a, Eq a) => EqProp (Average a) where
  a =-= b = property $ shouldBe a b


spec :: Spec
spec =
  describe "laws" $ do
    traverse_ (uncurry it) $ unbatch $ semigroup @(Average (Sum Int)) @Int undefined
    traverse_ (uncurry it) $ unbatch $ monoid @(Average (Sum Int)) undefined
    traverse_ (uncurry it) $ unbatch $ comonad @Average @(Int) @(Int) @(Int) undefined

