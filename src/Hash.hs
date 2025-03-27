module Hash where

import Data.Word
import Data.Monoid


class Hashable a where
  hashPrimitive :: a -> Word64 -> Word64

newtype Hash = Hash (Endo Word64)
  deriving newtype (Semigroup, Monoid)

hash :: Hashable a => a -> Hash
hash a = Hash $ Endo $ hashPrimitive a

getHashWithSalt :: Hash -> Word64 -> Word64
getHashWithSalt (Hash (Endo f)) = f
