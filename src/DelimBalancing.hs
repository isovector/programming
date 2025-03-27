module DelimBalancing where

data Balanced a
  = Unbalanced
  | Fragment [a] [a]
  deriving stock Show

instance Eq a => Semigroup (Balanced a) where
  Unbalanced <> _ = Unbalanced
  _ <> Unbalanced = Unbalanced
  Fragment c1 [] <> Fragment [] o2 = Fragment c1 o2
  Fragment c1 [] <> Fragment c2 o2 = Fragment (c1 <> c2) o2
  Fragment c1 o1 <> Fragment [] o2 = Fragment c1 (o2 <> o1)
  Fragment c1 (o : o1) <> Fragment (c : c2) o2
    | o == c = Fragment c1 o1 <> Fragment c2 o2
    | otherwise = Unbalanced

instance Eq a => Monoid (Balanced a) where
  mempty = Fragment [] []

data Delim = Paren | Brace | Bracket
  deriving stock (Eq, Show)

parse :: Char -> Balanced Delim
parse '(' = Fragment []        [Paren]
parse ')' = Fragment [Paren]   []
parse '[' = Fragment []        [Bracket]
parse ']' = Fragment [Bracket] []
parse '{' = Fragment []        [Brace]
parse '}' = Fragment [Brace]   []
parse _ = mempty
