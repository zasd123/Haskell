{-
#################################################################################
  Prog6.hs
  Zachary Hansen, April 10th,2019
#################################################################################
-}

module Prog6 where

--In the following functions, we are going to implement a binary tree using the following type definition:
data Tree1 = Leaf1 Int
           | Node1 Tree1 Int Tree1

--t1 test = [2,1,5,7,8]
t1:: Tree1
t1 = Node1 (Leaf1 1) 2 (Node1(Leaf1 7)5 (Leaf1 8))

t12:: Tree1
t12 = Node1 (Node1 (Leaf1 1)3 (Leaf1 2)) 2 (Leaf1 3)

t13:: Tree1
t13 = Node1 (Node1 (Node1 (Node1 (Leaf1 3)3(Leaf1 6))2 (Leaf1 5))3 (Leaf1 2)) 2 (Leaf1 3)

t3:: Tree1
t3 = Node1 (Leaf1 3)4(Leaf1 6)

--Write a function preorder that takes a tree argument and returns as a list an inorder traversal of the tree.
preorder :: Tree1 -> [Int]
preorder (Leaf1 x) = [x] 
preorder (Node1 left x right) = [x]++ preorder left ++ preorder right 

--Write a function postorder that takes a tree argument and returns as a list an inorder traversal of the tree.
postorder :: Tree1 -> [Int]
postorder (Leaf1 x) = [x]
postorder (Node1 left x right) = preorder left ++ preorder right ++ [x]

--Write a function sumPositives that takes a tree argument and returns the sum of positive integers in the tree.
sumpositives :: Tree1 -> Int
sumpositives (Leaf1 x) = x
sumpositives (Node1 left x right) = x + sumpositives left + sumpositives right


--Write a function countInteriorNodes that returns the number of interior nodes in the given tree.
countInteriorNodes :: Tree1 -> Int
countInteriorNodes (Leaf1 x) = 0
countInteriorNodes (Node1 left x right) = 1 + countInteriorNodes left + countInteriorNodes right

--Write a function depth that returns the depth of a tree. (A tree with only a root node is defined to have depth=1.)
depth :: Tree1 -> Int
depth (Leaf1 x) = 1
depth (Node1 left x right)
  |depth left == 1 =1 + depth right
  |depth right== 1 =1 + depth left

--In the following functions, we are going to implement a general tree, whose interior nodes can have an arbitrary number of children, using the following type definition:
data Tree2 a = Leaf2 a 
  | Node2 [Tree2 a]
                
t4:: Tree2 Int
t4 = Leaf2 6

t5:: Tree2 Int
t5 =  Node2 [(Leaf2 5), (Leaf2 4)]

t6:: Tree2 Int
t6= Node2 [(Leaf2 5), (Leaf2 4), (Leaf2 5), (Leaf2 4)]

t7:: Tree2 Int
t7 = Node2 [(Leaf2 5), (Leaf2 4), (Leaf2 5), (Leaf2 4), Node2 [(Leaf2 17)]]

t8:: Tree2 Int
t8 = Node2 [(Leaf2 5), (Leaf2 4), (Leaf2 5), (Leaf2 4), Node2 [(Leaf2 17),(Leaf2 18)]]

t9:: Tree2 Int
t9 = Node2 [(Leaf2 5), Node2 [(Leaf2 44)], (Leaf2 5), (Leaf2 4), Node2 [(Leaf2 17)]]


--flatten and member are both helper functions

flatten :: Tree2 a -> [a]
flatten (Node2 [])= []
flatten (Leaf2 x) = [x]
flatten (Node2 (x:xs)) = flatten x  ++ flatten (Node2 xs)

member ::Eq a=> a -> [a] -> Bool
member x [] = False
member x (y:ys)
  |x/=y = member x ys
  |x==y = True
  |otherwise = False
   
--Write a function occurs that returns whether a given argument is present in a given tree.
occurs :: Eq a => a -> Tree2 a -> Bool
occurs x ys =  member x (flatten ys)
                             



--Write a function countLeaves that takes a tree argument and returns the number of leaves in the tree.
countLeaves :: Tree2 a -> Int
countLeaves ys = length (flatten ys)



--Write a function sumTree that takes a tree of integers and returns the sum of all integers in the tree.
sumTree :: Tree2 Int -> Int
sumTree ys = sum (flatten ys)




--Write a function post2 that returns a postorder traversal of the nodes in the tree.
post2 :: Tree2 a -> [a]
post2 (Node2 [])= []
post2 (Leaf2 x) = [x]
post2 (Node2 (x:xs)) = post2  x ++ post2 (Node2 xs)



--Write a function depthK that returns all nodes that are at depth k in the tree. (A tree with only a root node is defined to have depth=1.) The order that the nodes are returned does not matter.
depthK :: Int -> Tree2 a -> [a]
depthK  1  (Leaf2 y) = [y]   
depthK  x  (Node2 []) = []
depthK  x  (Node2 ((Leaf2 y): ys)) =  (depthK (x) (Node2 ys))
depthK  x  (Node2 ((Node2 y): ys)) = ((depthK (x-1) (Node2 y)) ++ (depthK  (x-1) (Node2 ys))) 
depthK  _  _ = []



