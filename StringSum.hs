module StringSum where

import Text.Read
import Test.QuickCheck.Property

stringSum :: String -> Maybe Int
stringSum s = fmap sum $ sequence $ fmap readMaybe $ words s

prop_stringSum_onlyDigits :: [Int] -> Property
prop_stringSum_onlyDigits a = 
    stringSum (unwords (map show a)) === Just (sum a)

prop_stringSum_digitsAndSpaces :: [Int] -> Property
prop_stringSum_digitsAndSpaces a =
    stringSum (" " ++ (unwords (map show a)) ++ " " ++ (unwords (map show a))) === Just ((sum a) * 2)

prop_stringSum_digitsAndLetters :: [Int] -> Property
prop_stringSum_digitsAndLetters a = 
    stringSum ("a" ++ (unwords (map show a)) ++ "a") === Nothing