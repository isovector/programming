{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

module QuadTree where

import Data.Foldable
import Data.Maybe
import Data.Semigroup (stimesMonoid)
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

instance Foldable (Quad n) where
  foldMap f (Cell a) = f a
  foldMap f (Four a1 a2 a3 a4) =
    mconcat [foldMap f a1, foldMap f a2, foldMap f a3, foldMap f a4]

instance Traversable (Quad n) where
  sequenceA (Cell fa) = Cell <$> fa
  sequenceA (Four fa1 fa2 fa3 fa4) =
    Four <$> sequenceA fa1
         <*> sequenceA fa2
         <*> sequenceA fa3
         <*> sequenceA fa4

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

type MSB :: Nat -> Type
data MSB n where
  MSB_Z :: MSB Z
  MSB_S :: BranchBit n => MSB n -> MSB (S n)

deriving stock instance Eq (MSB n)
deriving stock instance Ord (MSB n)
deriving stock instance Show (MSB n)


shiftoutl :: forall n. Ix (S n) -> (Bool, Ix n)
shiftoutl (Ix (MSB_S msb') ix) = (testBit ix $ branchBit @n, Ix msb' ix)

type Ix :: Nat -> Type
data Ix n = Ix
  { ix_msb :: MSB n
  , getIx :: Integer
  }
  deriving stock (Eq, Ord, Show)




singleton :: forall n m. Monoid m => Ix n -> Ix n -> m -> Quad n m
singleton (Ix MSB_Z _) _ m = pure m
singleton x@(Ix (MSB_S _) _) y m =
  let (lr, x') = shiftoutl x
      (tb, y') = shiftoutl y
  in case (tb, lr) of
        (False, False) -> Four (singleton x' y' m) mempty mempty mempty
        (False, True) -> Four mempty (singleton x' y' m) mempty mempty
        (True, False) -> Four mempty  mempty (singleton x' y' m) mempty
        (True, True) -> Four mempty mempty mempty (singleton x' y' m)

set :: Ix n -> Ix n -> a -> Quad n a -> Quad n a
set x y a q = liftA2 appEndo (singleton x y $ Endo $ const a) q


getNaive :: Ix n -> Ix n -> Quad n a -> a
getNaive x y q = fromJust $ getLast $ fold $ (singleton x y $ Last . Just) <*> q

get :: Ix n -> Ix n -> Quad n a -> a
get _ _ (Cell a) = a
get x@(Ix (MSB_S _) _) y (Four tl tr bl br) =
  let (lr, x') = shiftoutl x
      (tb, y') = shiftoutl y
  in case (tb, lr) of
        (False, False) -> get x' y' tl
        (False, True) -> get x' y' tr
        (True, False) -> get x' y' bl
        (True, True) -> get x' y' br


toIx :: forall n. BranchBit n => Integer -> Maybe (Ix n)
toIx i =
  let msb = branchBit @n
      bound = getProduct $ stimesMonoid msb 2
   in case i < bound of
        True -> Just $ Ix mkMSB i
        False -> Nothing


type BranchBit :: Nat -> Constraint
class BranchBit n where
  branchBit :: Int
  mkMSB :: MSB n

instance BranchBit Z where
  branchBit = 0
  mkMSB = MSB_Z

instance BranchBit n => BranchBit (S n) where
  branchBit = branchBit @n + 1
  mkMSB = MSB_S mkMSB


type Full = S (S (S Z))

main :: IO ()
main = print $ getNaive (Ix mkMSB 4) (Ix mkMSB 0)$ singleton @Full (Ix mkMSB 3) (Ix mkMSB 0) "x"
