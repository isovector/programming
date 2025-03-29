{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module QuadTree where

import Data.Bits
import Data.Monoid
import Data.Kind

data Nat = Z | S Nat

type Quad :: Nat -> Type -> Type
data Quad n a where
  Cell :: a -> Quad n a
  Four :: Quad n a -> Quad n a
       -> Quad n a -> Quad n a
       -> Quad (S n) a

deriving stock instance Eq a => Eq (Quad n a)
deriving stock instance Ord a => Ord (Quad n a)
deriving stock instance Show a => Show (Quad n a)

instance Functor (Quad n) where
  fmap f (Cell a) = Cell (f a)
  fmap f (Four a1 a2 a3 a4) = Four (fmap f a1) (fmap f a2) (fmap f a3) (fmap f a4)

instance Applicative (Quad n) where
  pure = Cell
  liftA2 f (Cell a) (Cell b) = Cell $ f a b
  liftA2 f (Cell a) (Four b1 b2 b3 b4) =
    Four (fmap (f a) b1) (fmap (f a) b2) (fmap (f a) b3) (fmap (f a) b4)
  liftA2 f (Four  a1 a2 a3 a4) (Cell b) =
    Four (fmap (flip f b) a1) (fmap (flip f b) a2) (fmap (flip f b) a3) (fmap (flip f b) a4)
  liftA2 f (Four a1 a2 a3 a4) (Four b1 b2 b3 b4) =
    Four (liftA2 f a1 b1) (liftA2 f a2 b2) (liftA2 f a3 b3) (liftA2 f a4 b4)

deriving via Ap (Quad n) a instance Semigroup a => Semigroup (Quad n a)
deriving via Ap (Quad n) a instance Monoid a => Monoid (Quad n a)


shiftoutl :: forall n. BranchBit n => Ix (S n) -> (Bool, Ix n)
shiftoutl (Ix ix) = (testBit ix $ branchBit @n, Ix ix)

type Ix :: Nat -> Type
newtype Ix n = Ix { getIx :: Integer }
  deriving newtype (Eq, Ord, Show)


type Build :: Nat -> Constraint
class BranchBit n => Build n where
  singleton :: Monoid m => Ix n -> Ix n -> m -> Quad n m


instance Build Z where
  singleton _ _ = pure

instance Build n => Build (S n) where
  singleton x y m =
    let (lr, x') = shiftoutl x
        (tb, y') = shiftoutl y
    in case (tb, lr) of
          (False, False) -> Four (singleton x' y' m) mempty mempty mempty
          (False, True) -> Four mempty (singleton x' y' m) mempty mempty
          (True, False) -> Four mempty  mempty (singleton x' y' m) mempty
          (True, True) -> Four mempty mempty mempty (singleton x' y' m)

set :: Build n => Ix n -> Ix n -> a -> Quad n a -> Quad n a
set x y a q = liftA2 appEndo (singleton x y $ Endo $ const a) q


type BranchBit :: Nat -> Constraint
class BranchBit n where
  branchBit :: Int

instance BranchBit Z where
  branchBit = 0

instance BranchBit n => BranchBit (S n) where
  branchBit = branchBit @n + 1


type Full = S (S (S Z))

main :: IO ()
main = print $ singleton @Full (Ix 4) (Ix 0) "x"
