{-# LANGUAGE MultiWayIf #-}

module Life where

import Control.Comonad
import Control.Comonad.Store
import Data.Monoid

type V2 = (Sum Int, Sum Int)

neighbors :: ComonadStore V2 w => w a -> [a]
neighbors = experiment $ \v ->
  [ v <> ( 0, -1)
  , v <> ( 1, -1)
  , v <> ( 1,  0)
  , v <> ( 1,  1)
  , v <> ( 0,  1)
  , v <> (-1,  1)
  , v <> (-1,  0)
  , v <> (-1, -1)
  ]

neighborsAlive :: [Bool] -> Int
neighborsAlive = sum . fmap fromEnum


life :: ComonadStore V2 w => w Bool -> w Bool
life = extend $ \board ->
  let n = neighborsAlive $ neighbors board
   in if | n <= 1 -> False
         | n == 2 -> extract board
         | n == 3 -> True
         | otherwise -> False

