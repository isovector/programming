module OptionParsing where

import Data.Maybe
import Data.Monoid
import GHC.Generics

data Options = Options
  { verbosityLevel :: Sum Int
  , help :: Last Bool
  , flag1' :: Last Bool
  , flag2' :: Last Bool
  , other :: [String]
  }
  deriving stock (Generic, Show)
  deriving (Semigroup, Monoid) via Generically Options

parseFlag :: String -> Options
parseFlag "-v"         = mempty { verbosityLevel = 1  }
parseFlag "--help"     = mempty { help = pure True   }
parseFlag "--flag1"    = mempty { flag1' = pure True  }
parseFlag "--no-flag1" = mempty { flag1' = pure False }
parseFlag "--flag2"    = mempty { flag2' = pure True  }
parseFlag "--no-flag2" = mempty { flag2' = pure False }
parseFlag str          = mempty { other  = pure str   }

flag1, flag2 :: Options -> Bool
flag1 = fromMaybe False . getLast . flag1'
flag2 = fromMaybe True  . getLast . flag2'
