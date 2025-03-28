module GridPath where

import Data.Ord
import Control.Comonad
import Control.Comonad.Traced
import Data.Foldable

type Grid = (->) (Sum Int, Sum Int)

neighbors :: Ord a => Grid a -> [(Sum Int, Sum Int)]
neighbors g
  = fmap fst
  $ filter ((> trace (0, 0) g) . snd)
  $ fmap (\xy -> (xy, trace xy g))
    [ ((-1), 0)
    , (1, 0)
    , (0, (-1))
    , (0, 1)
    ]

sample :: (Functor f, ComonadTraced m w) => f m -> w a -> f a
sample f g = fmap (\m -> trace m g) f

solution :: Ord a => Grid a -> Grid (Int, [a])
solution g =
  kfix $ \xy final ->
    let ns = extend neighbors g
        dirs = trace xy ns
        choices = sample dirs final
        here = trace xy g
     in case choices of
            [] -> (1, [here])
            _ ->
              let (len, path) = maximumBy (comparing fst) choices
               in (len + 1, here : path)

test :: Grid Int
test =
    \(Sum x, Sum y) ->
      case (0 <= x && x < w && 0 <= y && y < h) of
        True -> array !! y !! x
        False -> -99999
  where
    array =
      [ [8, 7, 6, 5, 6, 9]
      , [4, 5, 6, 5, 1, 4]
      , [3, 2, 1, 5, 2, 3]
      ]
    h = length array
    w = length $ head array

