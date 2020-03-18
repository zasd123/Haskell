{-
#############################################################################################
  Prog5.hs
  Zachary Hansen, March 29th,2019
#############################################################################################
-}

module Prog5 where

import Data.List hiding (union,intersection)

--Write a function reverse' that reverses a list. You must use a case expression inside of your function definition. You may not use any built-in Haskell functions.
reverse' :: [a] -> [a]
reverse' (x:xs)  = case (x:xs)  of
                       [] -> []
                       [x]-> [x]
                       (x:xs)-> ((reverse' xs)++[x])


--Write a function isPalindrome that returns if some list can be read the same way forward and backward. (e.g. "12345" and "madam")
isPalindrome :: String -> Bool
isPalindrome str 
  |str == (reverse' str)= True
  |otherwise = False


--Write a function safeFindAfter that takes a string and a list of strings, and returns the remainder of list after the given string is found. 
--The function should be "safe", that is returning the Maybe type. The item is guaranteed to be in the list a maximum of one time. (Hint: what are the two relevant data constructors for this problem?)

safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter [] [[]] = Nothing
safeFindAfter x (y:ys) = Just  [y|y<-ys,y/=x ]


--Write a function member that checks whether the given item is present in the given set.
data Set = Set [Int]
           |EmptySet
    deriving Show

member :: Int -> Set -> Bool
member x (Set []) = False
member x (Set(y:ys))  
  |x/= y = member x (Set ys) 
  |x== y = True
  |otherwise = False


--Write a function size that returns the number of elements in a given set.
size :: Set -> Int
size (Set []) = 0
size (Set (_:ys)) = 1+ size (Set ys)
  

--Write a function add that inserts the given item into a set. (If the item is already in the set, simply return the set unmodified.) (Hint: you may want to program a helper function that takes two Sets and merges them into one.)
add :: Int -> Set -> Set
add x (Set []) = (Set [x])
add x (Set (ys)) 
  |member x (Set ys) == True = (Set (ys))
  |member x (Set ys) == False= (Set (x:ys))

--helper funtion for intersection   
remove :: Int -> Set -> Set
remove x (Set ys) = (Set [y| y<-ys, x/= y])  
  

--Write a function safeRemoveMax that removes the largest element from a set of integers.
safeRemoveMax :: Set -> Maybe Int
safeRemoveMax (Set []) = Nothing
safeRemoveMax (Set xs) = Just(maximum xs)


--Write a function equal that returns whether two sets are equal.
equal :: Set -> Set -> Bool
equal (Set[]) (Set(y:ys)) = False  
equal (Set(x:xs)) (Set [])= False
equal (Set []) (Set []) = True
equal (Set (x:xs)) (Set ys)
  |((member x (Set ys)==True) && (size(Set xs)>0)) = (equal (Set xs) (Set ys)) 
  |((member x (Set ys)) == False) = False
  |otherwise = True

--Write a function union that takes two sets and returns the union of both sets.
union :: Set -> Set -> Set
union (Set [])  (Set []) = (Set [])
union (Set [])  (Set [ys]) = (Set [ys])
union (Set [xs])  (Set []) = (Set [xs]) 
union (Set (x:xs)) (Set ys)
  | (size(Set xs)>0)  =   (union (Set xs) (add x (Set ys)))
  | otherwise =  (add x (Set  ys))


--Write a function intersection that takes two sets and returns the intersection of them.
intersection :: Set -> Set -> Set
--intersection (Set [])  (Set []) = (Set [])
--intersection (Set [])  (Set [ys]) = (Set [ys])
--intersection (Set [xs])  (Set []) = (Set [xs]) 
intersection (Set (x:xs)) (Set (ys))
  | size (Set xs)>0 = intersection(Set xs) (Set( [y|y<-ys, member x (Set ys)== True]))
-- | size (Set xs)>0 = intersection(Set xs) (Set( [y|y<-ys, member x (Set ys)== True]))   
  |otherwise = (Set ys)
