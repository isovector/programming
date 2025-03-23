module Checkers where

import Control.Comonad
import Test.QuickCheck
import Text.Show.Functions ()

import Test.QuickCheck.Checkers
import Test.QuickCheck.Instances.Char ()

comonad :: forall m a b c.
         ( Comonad m
         , Arbitrary (m a), EqProp (m a), Show (m a)
         , EqProp (b)
         , EqProp (m c)
         , CoArbitrary (m a)
         , CoArbitrary (m b)
         , Arbitrary b
         , Arbitrary c
         ) =>
         m (a,b,c) -> TestBatch
comonad = const ( "comonad laws"
              , [ ("left  identity", property leftP)
                , ("right identity", property rightP)
                , ("distribution" , property distribP)
                , ("extract . duplicate" , property extractP)
                , ("fmap extract . duplicate" , property extract2P)
                ]
              )
 where
   rightP  :: (m a -> b) -> m a -> Property
   rightP f ma = extract (extend f ma) =-= f ma

   leftP :: m a -> Property
   leftP ma = extend extract ma =-= ma

   distribP :: (m a -> b) -> (m b -> c) -> m a -> Property
   distribP g f ma = extend f (extend g ma) =-= extend (f . extend g) ma

   extractP :: m a -> Property
   extractP ma = extract (duplicate ma) =-= ma

   extract2P :: m a -> Property
   extract2P ma = fmap extract (duplicate ma) =-= ma
