{-# LANGUAGE TemplateHaskell #-}

module MerkleTree where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Comonad.Cofree

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

makeBaseFunctor [''Tree]

type Hash = Int


newtype Merkle f = Merkle
  { getMerkle :: Cofree (Base f) Hash
  }

summary :: Cofree f a -> a
summary (a :< _) = a

toMerkle :: Recursive f => (Base f Hash -> Hash) -> f -> Merkle f
toMerkle alg = fmap Merkle $ cata $ \base ->
  let hash = alg $ fmap summary base
   in hash :< base


