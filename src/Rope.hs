{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Rope where

import GHC.Exts
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

value :: Cofree f a -> f (Cofree f a)
value (_ :< f) = f

type Rope = Cofree StringTreeF Int



index :: Base StringTree Rope -> Int -> Maybe Char
index (LeafF str) ix = listToMaybe $ drop ix str
index (BranchF (lsz :< l) (_ :< r)) ix =
  case ix < lsz of
    True -> index l ix
    False -> index r $ ix - lsz

split :: Rope -> Int -> (Rope, Rope)
split r@(sz :< _) ix
  | ix >= sz = (r, mempty)
split (_ :< LeafF str) ix =
  let (top, bot) = splitAt ix str
    in (rope top, rope bot)
split (_ :< BranchF l@(lsz :< _) r) ix =
  case ix < lsz of
    True ->
      let (top, bot) = split l ix
       in (top, bot <> r)
    False ->
      let (top, bot) = split r $ ix - lsz
       in (l <> top, bot)

cofreeCata :: forall f a x. Functor f => (a -> f x -> x) -> Cofree f a -> x
cofreeCata f (a :< b) = f a $ fmap (cofreeCata @f f) b

toStr :: Rope -> String
toStr = cofreeCata $ \_ -> \case
  LeafF str -> str
  BranchF l r -> l <> r


instance Semigroup Rope where
  (0 :< _) <> r = r
  l <> (0 :< _) = l
  l@(lsz :< _) <> r@(rsz :< _) = lsz + rsz :< BranchF l r

instance Monoid Rope where
  mempty = rope ""

rope :: String -> Rope
rope s = length s :< LeafF s


instance IsString Rope where
  fromString = rope

test :: Rope
test = (("Hello " <> "my ") <> (("na" <> "me i") <> ("s" <> " Sandy"))) <> ""
