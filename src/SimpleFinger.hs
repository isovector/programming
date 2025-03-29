module SimpleFinger where

import Prelude hiding (length)

data Digit a
  = One a
  | Two a a
  deriving stock Show

data FingerTree a
  = Empty
  | Deep (Digit a) (FingerTree (a, a))
  deriving stock Show

insert :: a -> FingerTree a -> FingerTree a
insert a Empty = Deep (One a) Empty
insert a (Deep (One x) zs) = Deep (Two a x) zs
insert a (Deep (Two x y) zs) = Deep (One a) $ insert (x, y) zs

pop :: FingerTree a -> Maybe (a, FingerTree a)
pop Empty = Nothing
pop (Deep (One a) Empty) = Just (a, Empty)
pop (Deep (One x) zs) = do
  ((y, z), zs') <- pop zs
  pure (x, Deep (Two y z) zs')
pop (Deep (Two a x) zs) = Just (a, Deep (One x) zs)

length :: FingerTree a -> Int
length Empty = 0
length (Deep d f) = 2 * length f + case d of
  One _ -> 1
  Two _ _ -> 2
