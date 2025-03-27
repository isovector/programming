{-# LANGUAGE TemplateHaskell #-}

module MerkleTree where

import Hash
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Comonad.Cofree

data Tree a
  = Empty
  | Branch (Tree a) a (Tree a)


insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Branch Empty a Empty
insert a (Branch l x r) =
  case compare a x of
    EQ -> Branch l x r
    LT -> Branch (insert a l) x r
    GT -> Branch l x (insert a r)


makeBaseFunctor [''Tree]



type Merkle f = Cofree (Base f) Hash

summary :: Cofree f a -> a
summary (a :< _) = a

toMerkle :: (Recursive f, Foldable (Base f)) => f -> Merkle f
toMerkle = cata makeHash

makeHash :: (Foldable f, Monoid a) => f (Cofree f a) -> Cofree f a
makeHash base = foldMap summary base :< base

insert' :: Ord a => a -> Cofree (Base (Tree a)) Hash -> Cofree (Base (Tree a)) Hash
insert' a empty@(_ :< EmptyF) = makeHash $ BranchF empty a empty
insert' a res@(_ :< BranchF l x r) =
  case compare a x of
    EQ -> res
    LT -> makeHash $ BranchF (insert' a l) x r
    GT -> makeHash $ BranchF l x (insert' a r)

