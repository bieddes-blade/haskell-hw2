{-# LANGUAGE InstanceSigs #-}

module Parser where

{-import Control.Applicative
import Data.Char
import Data.Functor
import Test.QuickCheck.Poly-}

import Data.Either
import Test.QuickCheck
import Control.Arrow

newtype Parser s a = Parser{runParser :: [s] -> Either String (a, [s])}

-- | 10
instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (p) = Parser $ fmap (first f) . (runParser p)

-- | 11
instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser func
        where
            func s = Right (a, s)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    (Parser run1) <*> (Parser run2) = Parser combine
        where
            combine s = do
                (res1, end1) <- run1 s
                (res2, end2) <- run2 end1
                return (res1 res2, end2)

-- | 12
instance Monad (Parser s) where
    return :: a -> Parser s a
    return a = Parser func
        where
            func s = Right (a, s)

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    (Parser run) >>= f = Parser apply
        where
            apply s = do
                (res, end) <- run s
                runParser (f res) end

-- | 14
ok :: Parser s ()
ok = Parser dumb 
    where
        dumb s = Right ((), s)

prop_ok :: [Char] -> Property
prop_ok a =
    runParser ok a === Right ((), a)

-- | 15
eof :: (Eq s) => Parser s ()
eof = Parser is_eof 
    where
        is_eof s
            | (s == []) = Right ((), s)
            | otherwise = Left "I haven't reached the eof!"

prop_eof :: [Char] -> Property
prop_eof a = if (a == [])
    then runParser eof a === Right((), "")
    else isLeft (runParser eof a) === True

-- | 16
satisfy :: (s -> Bool) -> Parser s s
satisfy _p = Parser is_satisfied
    where 
        is_satisfied [] = Left "Stream is empty!"
        is_satisfied (a : as)
            | (_p a) = Right (a, as)
            | otherwise = Left "Predicate is not satisfied!"

prop_satisfy :: [Char] -> Property
prop_satisfy [] = isLeft (runParser (satisfy (== '0')) "") === True
prop_satisfy (c : s) = runParser (satisfy (== c)) (c : s) === Right(c, s)

-- | 17
element :: (Eq s) => s -> Parser s s
element _e = Parser is_elem
    where
        is_elem [] = Left "Stream is empty!"
        is_elem (a : as)
            | (a == _e) = Right (a, as)
            | otherwise = Left "Element is not desired!"

prop_element :: [Char] -> Property
prop_element [] = isLeft (runParser (element ('0')) "") === True
prop_element (c : s) = runParser (element (c)) (c : s) === Right(c, s)