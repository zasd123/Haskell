module Prog7 where

import Data.List
import Data.Maybe

{-
#######################################################################
	Zachary Hansen
	Prog7.hs April 25th, 2019
#######################################################################

-}


--Write a function unique that returns the list of elements that occur exactly once in the argument list. You must use recursion and not list comprehension. A helper function, or functions, may be useful.
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) 
  |(length (elemIndices x xs))  > 0  = unique (remove x xs)  
  |(length (elemIndices x xs)) == 0  = x: (unique xs) 
  
remove :: Eq a => a-> [a] -> [a]
remove y [] = []
remove y (x:xs) 
  |y==x  = remove y xs
  |otherwise = x : (remove y xs) 


--Consider the following type:
data Expr1 = Val1 Int
           | Add1 Expr1 Expr1
           | Sub1 Expr1 Expr1

--Write a function value1 that evaluates an expression.
value1 :: Expr1 -> Int
value1 (Val1 x) = x
value1 (Add1 x y) = (value1  x) + (value1 y)
value1 (Sub1 x y) = (value1  x) - (value1 y)


--Create a Expr2 type constructor that also supports multiplication and division, in addition to the int literal, addition, and subtraction.
data Expr2 = Val2 Int 
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Mul2 Expr2 Expr2
           | Div2 Expr2 Expr2


--Write a function value2 that evaluates an expression, but returns Nothing if there is a division by zero scenario.
value2 :: Expr2 -> Maybe Int
value2 (Val2 x) = Just x
value2 (Add2 x y)   = madd (value2  x) (value2 y)
value2 (Sub2 x y)   = msub (value2  x) (value2 y)
value2 (Mul2 x y) = mmul (value2  x) (value2 y)
value2 (Div2 x y) 
  |mdiv (value2  x) (value2 y)== Just 0 = Nothing
  |otherwise = mdiv (value2 x) (value2 y)


madd :: Maybe Int -> Maybe Int -> Maybe Int
madd x y = Just (fromJust  x + fromJust  y) 

msub :: Maybe Int -> Maybe Int -> Maybe Int
msub x y = Just (fromJust  x - fromJust  y)

mmul :: Maybe Int -> Maybe Int -> Maybe Int
mmul x y = Just (fromJust  x * fromJust  y)

mdiv :: Maybe Int -> Maybe Int -> Maybe Int
mdiv x y = Just (fromJust  x `div` fromJust  y)



--Make the Expr2 type an instance of the Show class. Appropriate define the function show so that (Add2 (Val2 3) (Val2 4)) returns the string "3 + 4".
instance Show Expr2 where
  show (Val2 x) = show x
  show (Add2 x y) ="(" ++ show x ++ "+" ++ show y ++ ")"
  show (Sub2 x y) ="(" ++ show x ++ "-" ++ show y ++ ")"
  show (Mul2 x y) ="(" ++ show x ++ "*" ++ show y ++ ")"
  show (Div2 x y) ="(" ++ show x ++ "/" ++ show y ++ ")"



--show :: Expr2 a -> String 




--Write a function piglatinize that returns a word into its piglatin form: if it begins with a vowel, add to the end "yay", else move non-vowels to the end of the string until a vowel is at the front and then add to the end "ay". The word arguments are guaranteed to have a vowel (a, e, i, o, or u) and not begin with the letter y.
piglatinize :: String -> String
piglatinize (x:xs)
  |x== 'a' || x== 'a'|| x== 'a'|| x== 'a' ||x== 'a' = (x:xs) ++ ['y', 'a', 'y']
  |otherwise = moveVow(x:xs)

moveVow :: String -> String
moveVow (x:xs)
 | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'  = (x:xs) ++ ['a', 'y']
 |otherwise = (moveVow xs)++ [x]



--Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)

--A tree is balanced if the number of leaves in the left and right subtree of every node differ by at most one. Write a function balanced that returns whether a tree is balanced or not.
balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) =((treeSize left)- (treeSize right)) <= 1 && (balanced left) && (balanced right)

treeSize ::Tree a -> Int
treeSize (Leaf x) = 1
treeSize (Node left right) = 1 + (treeSize left) + (treeSize right)
treeSize _ = 0



--Now we will extend the Expr example above to contain conditional expressions. Take everything from Expr2, and create an Expr3, like so:
data Expr3 = Val3 Int
           | Add3 Expr3 Expr3
           | Sub3 Expr3 Expr3
           | Mul3 Expr3 Expr3
           | Div3 Expr3 Expr3
           | If BExpr3 Expr3 Expr3



--The If data constructor (If BExpr3 Expr3 Expr3) will evaluate the boolean expression (first argument) and will return the value of the second argument if it is true, else it will return the third argument. Define the BExpr3 type as the following:
data BExpr3 = BoolLit Bool
            | Or BExpr3 BExpr3
            | EqualTo Expr3 Expr3
            | LessThan Expr3 Expr3


--Write a function bEval :: BExpr3 -> Bool that evaluates instances of the above boolean expression.
bEval :: BExpr3 -> Bool
bEval (BoolLit x ) = x
bEval (Or x y) = (bEval x) || (bEval y)
bEval (EqualTo x y) = (value3 x) == (value3 y)
bEval (LessThan x y)= (value3 x) < (value3 y) 


--Write a function value3 that evaluates an expression.
value3 :: Expr3 -> Maybe Int
value3 (Val3 x) = Just x
value3 (Add3 x y) = (vAdd (value3 x) (value3 y))
value3 (Sub3 x y) = (vSub (value3 x) (value3 y))
value3 (Mul3 x y) = (vMul (value3 x) (value3 y))
value3 (Div3 x y) = (vDiv (value3 x) (value3 y))
value3 (If b x y)= (sb (b) (value3 x) (value3 y))


vAdd :: Maybe Int -> Maybe Int -> Maybe Int
vAdd x y = Just ((fromJust x) + (fromJust y))

vSub :: Maybe Int -> Maybe Int -> Maybe Int
vSub x y = Just ((fromJust x) - (fromJust y))

vMul :: Maybe Int -> Maybe Int -> Maybe Int
vMul x y = Just ((fromJust x) * (fromJust y))


vDiv :: Maybe Int -> Maybe Int -> Maybe Int
vDiv x (Just 0) = Nothing
vDiv x y = Just ((fromJust x) `div` (fromJust y))

sb :: BExpr3 -> Maybe Int -> Maybe Int -> Maybe Int
sb b x y
  |bEval b = (Just (fromJust x))
  |otherwise = (Just (fromJust y))
