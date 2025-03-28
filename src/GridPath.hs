module GridPath where

import Data.Ord
import Control.Comonad
import Data.Foldable

newtype Grid a = Grid
  { getGrid :: Int -> Int -> a }
  deriving stock Functor

instance ComonadApply Grid where
  Grid f <@> Grid a = Grid $ \x y -> f x y (a x y)


instance Comonad Grid where
  extract g = getGrid g 0 0
  duplicate (Grid g) = Grid $ \x0 y0 -> Grid $ \x y -> g (x + x0) (y + y0)

neighbors :: Ord a => Grid a -> [(Int, Int)]
neighbors (Grid g)
  = fmap fst
  $ filter ((> g 0 0) . snd)
  $ fmap (\(x, y) -> ((x,y), g x y))
    [ ((-1), 0)
    , (1, 0)
    , (0, (-1))
    , (0, 1)
    ]

move :: Int -> Int -> Grid a -> Grid a
move x y g = getGrid (duplicate g) x y

sample :: Int -> Int -> Grid a -> a
sample x y = extract . move x y

follow :: Functor f => f (Int, Int) -> Grid a -> f a
follow f g = fmap (\(x, y) -> sample x y g) f

solution :: Ord a => Grid a -> Grid (Int, [a])
solution g =
  kfix $ Grid $ \x y final ->
    let ns = extend neighbors g
        dirs = sample x y ns
        choices = follow dirs final
        here = sample x y g
     in case choices of
            [] -> (1, [here])
            _ ->
              let (len, path) = maximumBy (comparing fst) choices
               in (len + 1, here : path)



test :: Grid Int
test =
    Grid $ \x y ->
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

