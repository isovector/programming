{-# LANGUAGE TemplateHaskell #-}

module Rope where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Comonad.Cofree
import Data.Maybe

data StringTree
  = Leaf String
  | Branch StringTree StringTree



makeBaseFunctor [''StringTree]


summary :: Cofree f a -> a
summary (a :< _) = a

type Rope = Cofree StringTreeF Int



index :: Base StringTree Rope -> Int -> Maybe Char
index (LeafF str) ix = listToMaybe $ drop ix str
index (BranchF (lsz :< l) (_ :< r)) ix =
  case ix <= lsz of
    True -> index l ix
    False -> index r $ ix - lsz

instance Semigroup Rope where
  (0 :< _) <> r = r
  l <> (0 :< _) = l
  l@(lsz :< _) <> r@(rsz :< _) = lsz + rsz :< BranchF l r

instance Monoid Rope where
  mempty = 0 :< LeafF ""

