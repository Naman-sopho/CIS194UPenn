module Homework3.Homework3 where

import Data.List

-- zipWith func list1 list2
-- replicate (length list) list
-- filter(\x -> x mod n == 0) []
skip :: Int -> [a] -> [a]
skip n = map snd . filter (\x -> mod (fst x) n == 0) . zip [1..]

skips :: [a] -> [[a]]
skips xs = zipWith ($) (map skip [1..]) $ replicate (length xs) xs

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs)
  | (b > c) && (b > a) = b : localMaxima (b : c : xs)
  | otherwise = localMaxima (b : c : xs)

localMaxima _ = []