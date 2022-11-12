{-# LANGUAGE InstanceSigs #-}

module NonEmpty where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Poly

data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

-- | 3
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (a :| as) = f a :| (fmap f as)

prop_NonEmpty_fmaps_as_a_list :: Fun A B -> NonEmpty A -> Property
prop_NonEmpty_fmaps_as_a_list (Fun _ f) xs =
  toList (fmap f xs) === fmap f (toList xs)

prop_Functor_Identity :: NonEmpty A -> Property
prop_Functor_Identity x =
  fmap id x === x

prop_Functor_Composition :: Fun B C -> Fun A B -> NonEmpty A -> Property
prop_Functor_Composition (Fun _ f) (Fun _ g) x =
  fmap (f . g) x === (fmap f . fmap g) x

-- | 4
instance Applicative NonEmpty where
  pure a = a :| []
  {-fs <*> as = x :| xs 
    where
      x : xs = [f a | f <- (toList fs), a <- (toList as)]-}
  (<*>) = ap

prop_Applicative_Identity :: NonEmpty A -> Property
prop_Applicative_Identity v =
  (pure id <*> v) === v

prop_Applicative_Composition :: NonEmpty (Fun B C) -> NonEmpty (Fun A B) -> NonEmpty A -> Property
prop_Applicative_Composition u' v' w =
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))
  where
    u = applyFun <$> u'
    v = applyFun <$> v'

-- | 5
instance Monad NonEmpty where
  (a :| as) >>= f = 
    let 
      b :| bs = f a
      cs = as >>= toList . f
    in b :| (bs ++ cs)

prop_Monad_LeftIdentity :: A -> Fun A (NonEmpty B) -> Property
prop_Monad_LeftIdentity a (Fun _ k) =
  (return a >>= k) === k a

prop_Monad_RightIdentity :: NonEmpty B -> Property
prop_Monad_RightIdentity m =
  (m >>= return) === m

prop_Monad_Associativity :: NonEmpty A -> Fun A (NonEmpty B) -> Fun B (NonEmpty C) -> Property
prop_Monad_Associativity m (Fun _ k) (Fun _ h) =
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

-- | 6
instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (a :| as) = f a (foldr f z as)

prop_Foldable_foldr :: [Int] -> Property
prop_Foldable_foldr input =
  foldr (+) 0 (0 :| input) === foldr (+) 0 (0 : input)

-- | 7
instance Traversable NonEmpty where
  traverse f (a :| as) = fmap (:|) (f a) <*> traverse f as

prop_traverse_Identity :: NonEmpty A -> Property
prop_traverse_Identity x =
  traverse Identity x === Identity x

prop_traverse_Composition :: Fun A (F B) -> Fun B (G C) -> NonEmpty A -> Property
prop_traverse_Composition (Fun _ f) (Fun _ g) x =
  traverse (Compose . fmap g . f) x
    === (Compose . fmap (traverse g) . traverse f) x

type F = Maybe

type G = Either String
