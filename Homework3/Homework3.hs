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

histo:: Int -> (Int, Int) -> String
histo m (i, n) = show i ++ "=" ++
            replicate n '*' ++
            replicate (m - n) ' '

histogram:: [Integer] -> String
histogram xs = let count = map (\n -> length $ filter (== n) xs) [0..9]
                   m = maximum count in
                   unlines $ reverse $ transpose $ map (histo m) $ zip [0..9] count