{-
###############################################################################################################################
  Prog2.hs
  Zachary Hansen, February 19th, 2019
###############################################################################################################################
-}
module Prog2 where

--This function returns the sum of all numbers from 1 to n.
sum' :: Integer -> Integer
sum' x
  |x==0 = 1
  |x>0 = sum'(x-1)+x


--This function returns the integer square root of the positive integer n.
integerSqrt :: Integer -> Integer
integerSqrt x 
  |x == (floor((sqrt (fromIntegral x)))^2) = floor(sqrt (fromIntegral x))
  |otherwise = error "The number you input does not have a non-fractional square root"  


--This function returns the result of raising some base number, x, to some exponent, y.
exponent' :: Integer -> Integer -> Integer
exponent'  x y
  |y == 1  = x
  |y  >  1  = (exponent' x (y-1)) * x


--This function replaces the built in infix || operator .
or' :: Bool -> Bool ->Bool
or' False False = False
or' _ _ = True


--This function returns a list of Integers in decending order.
--It uses the helper functions minOfThree, middleOfThree, and maxOfThree.
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x,y,z) = (maxOfThree(x,y,z),middleOfThree(x,y,z),minOfThree(x,y,z))


--This function returns the max of three Integers.
--It is a helper funciton for order triple.
maxOfThree :: (Integer, Integer, Integer) -> Integer
maxOfThree (x,y,z)
  |x == y && x > z = x
  |y == z && y > x = y
  |z == x && z > y = z
  |x > y && x > z = x
  |y > x && y > z = y
  |z > x && z > y = z
  |otherwise = x


--This function returns the middle of three Integers.
--It is a helper funciton for order triple.
middleOfThree :: (Integer, Integer, Integer) -> Integer
middleOfThree (x,y,z)
  |x == y  = x
  |y == z  = y
  |z == x  = z
  |x < z && x > y = x
  |y < z && y > x = y
  |z < x && z > y = z
  |otherwise = x


--This function returns the min  of three Integers.
--It is a helper funciton for order triple.
minOfThree :: (Integer, Integer, Integer) -> Integer
minOfThree (x,y,z)
  |x == y && x < z = x
  |y == z && y < x = y
  |z == x && z < y = z
  |x < y && x < z = x
  |y < x && y < z = y
  |z < x && z < y = z
  |otherwise = x

--This function returns the characters in a quadruple (4-tuple) in the following way:
--The first elements swaps with the last, and the middle two flip. 
swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (w,x,y,z) = (z,y,x,w)


--This function returns a list of the ascii values of characters in a string.
asciiNums :: String -> [Int]
asciiNums  nums  = [(fromEnum x) |x <- nums, nums /= [ ]] 


--This function returns all instances of an integer n from a list
matches :: Integer -> [Integer] -> [Integer]
matches y  xs  = [x | x <- xs, x==y] 
  

--This function uses  the matches function in the above problem to write a function element that
--returns True if an element is a member of a list, False otherwise. 
element :: Integer -> [Integer] -> Bool
element  x xs 
  |(matches x xs) /= [] = True
  |otherwise = False
 
