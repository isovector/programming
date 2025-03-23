module Derivative where

import GHC.Generics
import Data.Ratio
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Checkers
import Data.Foldable
import Checkers
import Test.Hspec
import Control.Comonad
import Test.QuickCheck (CoArbitrary, (===), property)
import Data.Monoid

newtype Fn a b = Fn { sample :: a -> b }
  deriving stock Functor
  deriving newtype (Semigroup, Monoid, Arbitrary, CoArbitrary)

instance Show (Fn a b) where
  show _ = "Fn"

instance Num a => Comonad (Fn a) where
  extract (Fn f) = f 0
  duplicate (Fn f) = Fn $ \a0 -> Fn $ \x -> f $ x + a0


data R2 a = R2 a a
  deriving stock (Generic1)
  deriving (Functor, Applicative) via Generically1 R2
  deriving (Num, Fractional) via Ap R2 a

instance (Applicative f, Fractional a) => Fractional (Ap f a) where
  fromRational = pure . fromRational
  recip = fmap recip



epsilon :: Fractional a => a
epsilon = 1 / 50000000


test1 :: Fn (Ratio Integer) (Ratio Integer)
test1 = Fn $ \x -> x * x * x

translate :: Num a => a -> Fn a b -> Fn a b
translate a = extend $ \fn -> sample fn a

derivative :: (Fractional a, Fractional b) => Fn a b -> Fn a b
derivative = extend $ \fn -> (sample fn epsilon - sample fn (-epsilon)) / (2 * epsilon)

test :: Fn (Ratio Integer) b -> b
test fn = sample fn 0

--------------------------------------------------------------------------------


instance (Eq b, Show b, Arbitrary a, Show a) => EqProp (Fn a b) where
  Fn a =-= Fn b = property $ \x -> a x === b x

spec :: Spec
spec =
  describe "laws" $ do
    traverse_ (uncurry it) $ unbatch $ comonad @(Fn (Ratio Integer)) @((Ratio Integer)) @((Ratio Integer)) @((Ratio Integer)) undefined
