module Homework1 where

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev num = if num < 0
                then []
                else num `mod` 10 : toDigitsRev (num `div` 10)

toDigits :: Integer -> [Integer]
toDigits num = reverse (toDigitsRev num)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [2 * x]
doubleEveryOther list = doubleEveryOther (init (init list)) ++ [last (init list)] ++ [2 * (last list)]