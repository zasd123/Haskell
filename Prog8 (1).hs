

{-
##########################################################################
	Zachary Hansen
	April 28th, 2019
##########################################################################
-}

module Prog8 where

import Data.List

--Write a function sumSqNeg that computes the "sum of squares of negatives". You must use one or more higher-order functions: map, filter, foldr
sumSqNeg :: [Int] -> Int
sumSqNeg  xs = foldr (+)0 ([x*x| x<-xs, x<0])
    
--Write a function containing (without any higher order functions) that returns whether each element in the first list is also in the second list.
containing :: Eq a => [a] -> [a] -> Bool
containing [] ys = True
containing  (x:xs) ys
  |x `elem` ys = containing xs ys
  |otherwise = False


--Write a function total that applies the function (first argument) to every element in the list (second argument) and sums the result. You must use one or more higher-order functions: map, filter, foldr
total :: (Int -> Int) -> [Int] -> Int
total func xs = foldr (+) 0 (map func xs) 


--Write a function containing' (with higher order functions) that returns whether each element in the first list is also in the second list. You must use one or more higher-order functions: map, filter, foldr

containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = foldr (&&) True [x `elem` ys | x <- xs]


--Write a function lengths that returns a list of lengths of the given strings. You must use one or more higher-order functions: map, filter, foldr.
lengths :: [String] -> [Int]
lengths xs = map length xs


--Write a function product' that returns the product of a nonempty list of numbers. You must use one or more higher-order functions: map, filter, foldr.
product' :: Num a => [a] -> a
product' [] = 0
product' xs = foldr (*) 1 xs


--Write a function max' that returns the largest element of a nonempty list. You must use one or more higher-order functions: map, filter, foldr.
max' :: Ord a => [a] -> a
max' [] = error "empty list"
max' (x:xs) = foldr max x xs 

--Write a function append' that appends two lists. You must use one or more higher-order functions: map, filter, foldr.
append' :: [a] -> [a] -> [a]
append' xs [] = xs
append' [] ys = ys
append' xs ys = foldr (:) ys xs



--Write a function filterFirst that removes the first element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst func [] = []
filterFirst func xs 
  | length (filter func [(head xs)]) == 0 = (tail xs)
  | otherwise = head(xs):filterFirst func (tail xs) 
 

 
--Write a function filterLast that removes the last element from the list (second argument) that does not satisfy a given predicate function (first argument). You must use one or more higher-order functions: map, filter, foldr.
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast func [] = []
filterLast func xs = reverse (filterFirst func (reverse xs))
