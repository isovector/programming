{-# LANGUAGE MultiWayIf #-}

module ParenBalancing where


data Balance = Balance
  { close :: Int
  , open :: Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup Balance where
  Balance c1 0 <> Balance c2 o2 =
    Balance (c1 + c2) o2
  Balance c1 o1 <> Balance 0 o2 =
    Balance c1 (o1 + o2)
  Balance c1 o1 <> Balance c2 o2 =
    case compare o1 c2 of
      EQ -> Balance c1 o2
      LT -> Balance (c1 + c2 - o1) o2
      GT -> Balance c1 (o2 + c1 - o2)


instance Monoid Balance where
  mempty = Balance 0 0

delimited :: Eq a => a -> a -> a -> Balance
delimited l r = \c ->
  if | c == l    -> Balance 0 1
     | c == r    -> Balance 1 0
     | otherwise -> mempty

paren :: Char -> Balance
paren = delimited '(' ')'
