{-
##########################################################################################
  Prog3.hs
  Zachary Hansen, February 19th, 2019
##########################################################################################
-}

module Prog3 where

import Data.Char
--Write a function sumLastPart which, only using library functions, returns the sum of the last n numbers in the list, where n is the first argument to the funciton
sumLastPart :: Int ->  [Int] -> Int
sumLastPart y  nums  
  |nums /= [] = sum((drop((length nums)-y) nums)) 
  |otherwise = error "Invalid Input"

--Write a function init' that has identical behavior to the init function. In your definition, you may only use the standard Haskell functions that operate on lists (except for init).
init' :: [Int] -> [Int]
init' nums
  |nums /= [] = (take((length nums) -1) nums)
  |otherwise = error "Invalid Input"

--Write a recursive function init'' that has the same behavior as init'. No standard Haskell functions may be used.
init'' :: [Int] -> [Int]
init'' [x] = [ ]
init'' (x:xs) = x:init'' xs 

--Write a recursive function elemAt that returns the ith item of the list, where the first item is index 1. You may not use any of the standard Haskell functions that operate on lists.
elemAt :: Int -> [Int] -> Int 
elemAt 0 (x:xs) = x
elemAt y (x:xs) = (elemAt (y-1) xs)  
elemAt _ _ = error "Invalid Index"

--Write a function numTimes that returns the number of times that an element occurs in the list. Use recursion, not a list comprehension.
numTimes :: Int -> [Int] -> Int
numTimes y (x:xs) 
  |y==x =  (1+ (numTimes y xs))
  |y/=x =  (0+ (numTimes y xs))
numTimes y _ = 0

--Write a function lowerFirstLetter that lowercases the first and only first letter of a string.
lowerFirstLetter :: String -> String
lowerFirstLetter (x:xs)
  | xs /= [] = ((toLower x):xs)
  |otherwise = error "There was a problem" 	

--Write a function and' that uses recursion to return the conjunction of a list of boolean values. 
and' :: [Bool] -> Bool
and' [True,True] = True
and' [_ , _] = False
and' (x:xs) = and' xs  
 
--Write a function or' that uses recursion to return the disjunction of a list of boolean values.
or' :: [Bool] -> Bool
or' [False,False] = False
or' [_ , _] = True
or' (x:xs)= or' xs

--Write a function iSort' that uses insertion sort to sort a list of triples (Float, Int, String) where only the second element (the Int part of the triple) is to be considered during the sorting process.
iSort' :: [(Float,Int,String)] -> [(Float,Int,String)]
iSort' [] = []
iSort' (x:xs) = ins x (iSort' xs)


ins :: (Float,Int,String) -> [(Float, Int, String)] -> [(Float,Int,String)]
ins (x,y,z) [ ] = [(x,y,z)]
ins (x,y,z) ((x1,y1,z1):xs)
  |y < y1 = ((x,y,z):(x1,y1,z1):xs)
  |otherwise = ((x1,y1,z1):(ins (x,y,z) xs))

--Write a function merge that takes two sorted lists (decreasing order) and merges them into a single sorted list (decreasing order).
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] (y:ys)= []
merge (x:xs) [] = []
merge (x:xs) (y:ys) 
  |x >= y  =  ((x:[y]) ++ (merge xs ys))
  |y >= x  =  ((y:[x]) ++ (merge xs ys))


 
