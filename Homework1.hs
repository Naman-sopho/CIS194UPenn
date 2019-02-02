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

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate x = if sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0
             then True
             else False