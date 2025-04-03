{-# LANGUAGE MultiWayIf #-}

module Window where

import Data.Foldable
import Control.Comonad
import Control.Comonad.Store
import Data.Monoid

data Stream a = Stream a (Stream a)
  deriving stock (Functor, Foldable)


instance Applicative Stream where
  pure a = Stream a $ pure a
  liftA2 f (Stream a as) (Stream b bs) = Stream (f a b) $ liftA2 f as bs

instance Comonad Stream where
  extract (Stream a _) = a
  duplicate s@(Stream _ as) = Stream s $ duplicate as

window :: Int -> ([a] -> b) -> Stream a -> Stream b
window n f = extend (f . takeS n)

takeS :: Int -> Stream a -> [a]
takeS n = take n . toList


nats :: Stream Int
nats = Stream 0 $ fmap (+ 1) nats
