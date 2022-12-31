
---- Part 1: Basic structural recursion ----------------

-- 1. Merge sort

-- Deal a list into two (almost) equal-sizes lists by alternating elements
-- For example, deal [1,2,3,4,5,6,7] = ([1,3,5,7], [2,4,6])
-- and          deal   [2,3,4,5,6,7] = ([2,4,6], [3,5,7])
-- Hint: notice what's happening between the answers to deal [2..7] and
-- deal (1:[2..7]) above to get an idea of how to approach the recursion
deal :: [a] -> ([a],[a])
deal [] = ([],[])
deal (x:xs) = let (ys,zs) = deal xs
              in (x:zs, ys)

-- Now implement merge and mergesort (ms), and test with some
-- scrambled lists to gain confidence that your code is correct
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys 
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = (x: merge xs (y:ys))
  | x > y  = (y: merge (x:xs) ys)

ms :: Ord a => [a] -> [a]
ms [] = []
ms [x] = [x]
ms xs = merge (ms (take s xs)) (ms (drop s xs))
      where s = (length xs) `div` 2    -- general case: deal, recursive call, merge

-- 2. A backward list data structure 

-- Back Lists: Lists where elements are added to the back ("snoc" == rev "cons")
-- For example, the list [1,2,3] is represented as Snoc (Snoc (Snoc Nil 1) 2) 3
data BList a = Nil | Snoc (BList a) a deriving (Show,Eq)

-- Add an element to the beginning of a BList, like (:) does
cons :: a -> BList a -> BList a
cons e Nil = Snoc Nil e
cons e (Snoc Nil t) = Snoc (Snoc Nil e) t
cons e (Snoc bl t) = Snoc (cons e bl) t

-- Convert a usual list into a BList (hint: use cons in the recursive case)
toBList :: [a] -> BList a
toBList [] = Nil
toBList [e] = Snoc Nil e
toBList (e:rs) = cons e (toBList rs)

-- Add an element to the end of an ordinary list
snoc :: [a] -> a -> [a]
snoc [] e = [e]
snoc (h:rs) e = h:(snoc rs e)

-- Convert a BList into an ordinary list (hint: use snoc in the recursive case)
fromBList :: BList a -> [a]
fromBList Nil = []
fromBList (Snoc bl t) = snoc (fromBList bl) t

-- 3. A binary tree data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Count number of Empty's in the tree
num_empties :: Tree a -> Int
num_empties Empty = 1
num_empties (Node a t1 t2) = (num_empties t1) + (num_empties t2)

-- Count number of Node's in the tree
num_nodes :: Tree a -> Int
num_nodes Empty = 0
num_nodes (Node a t1 t2) = 1 + (num_nodes t1) + (num_nodes t2)

-- Insert a new node in the leftmost spot in the tree
insert_left :: a -> Tree a -> Tree a
insert_left x Empty = Node x Empty Empty
insert_left x (Node a t1 t2) = Node a (insert_left x t1) t2

-- Insert a new node in the rightmost spot in the tree
insert_right :: a -> Tree a -> Tree a
insert_right x Empty = Node x Empty Empty
insert_right x (Node a t1 t2) = Node a (insert_right x t2) t1

-- Add up all the node values in a tree of numbers
sum_nodes :: Num a => Tree a -> a
sum_nodes Empty = 0
sum_nodes (Node a t1 t2) = (sum_nodes t1) + a + sum_nodes t2

-- Produce a list of the node values in the tree via an inorder traversal
-- Feel free to use concatenation (++)
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a t1 t2) = (inorder t1) ++ [a] ++ (inorder t2)

-- 4. A different, leaf-based tree data structure
data Tree2 a = Leaf a | Node2 a (Tree2 a) (Tree2 a) deriving Show

-- Count the number of elements in the tree (leaf or node)
num_elts :: Tree2 a -> Int
num_elts (Leaf x) = 1
num_elts (Node2 a left right) = 1 + num_elts left + num_elts right

-- Add up all the elements in a tree of numbers
sum_nodes2 :: Num a => Tree2 a -> a
sum_nodes2 (Leaf a) = a
sum_nodes2 (Node2 a left right) = a + (sum_nodes2 left) + (sum_nodes2 right)

-- Produce a list of the elements in the tree via an inorder traversal
-- Again, feel free to use concatenation (++)
inorder2 :: Tree2 a -> [a]
inorder2 (Leaf a) = [a]
inorder2 (Node2 a left right) = (inorder2 left) ++ [a] ++ (inorder2 right)

-- Convert a Tree2 into an equivalent Tree1 (with the same elements)
conv21 :: Tree2 a -> Tree a
conv21 (Leaf a) = Node a Empty Empty 
conv21 (Node2 a left right) = (Node a (conv21 left) (conv21 right))

---- Part 2: Iteration and Accumulators ----------------


-- Both toBList and fromBList from Part 1 Problem 2 are O(n^2) operations.
-- Reimplement them using iterative helper functions (locally defined using
-- a 'where' clause) with accumulators to make them O(n)
toBList' :: [a] -> BList a
toBList' xs = toBList_it xs Nil where
  toBList_it [] a = a
  toBList_it (x:xs) a = toBList_it xs (Snoc a x)

fromBList' :: BList a -> [a]
fromBList' xs = fromBList_it xs [] where
  fromBList_it Nil a = a
  fromBList_it (Snoc xs x) a = fromBList_it xs (x:a)


-- Even tree functions that do multiple recursive calls can be rewritten
-- iteratively using lists of trees and an accumulator. For example,
sum_nodes' :: Num a => Tree a -> a
sum_nodes' t = sum_nodes_it [t] 0 where
sum_nodes_it :: Num a => [Tree a] -> a -> a
sum_nodes_it [] a = a
sum_nodes_it (Empty:ts) a = sum_nodes_it ts a
sum_nodes_it (Node n t1 t2:ts) a = sum_nodes_it (t1:t2:ts) (n+a)

-- Use the same technique to convert num_empties, num_nodes, and sum_nodes2
-- into iterative functions with accumulators

num_empties' :: Tree a -> Int
num_empties' t = num_empties_it [t] 0 where
  num_empties_it:: [Tree a] -> Int -> Int
  num_empties_it [] x = x 
  num_empties_it (Empty:ts) a = num_empties_it ts (a+1)
  num_empties_it (Node n t1 t2:ts) a = num_empties_it (t1:t2:ts) a

num_nodes' :: Tree a -> Int
num_nodes' t = num_nodes_it [t] 0 where 
  num_nodes_it::[Tree a] -> Int -> Int 
  num_nodes_it [] a = a 
  num_nodes_it (Empty:ts) a = num_nodes_it ts (a+1)
  num_nodes_it (Node n t1 t2:ts) a = num_nodes_it (t1:t2:ts) (a+1)

sum_nodes2' :: Num a => Tree2 a -> a
sum_nodes2' t = sum_nodes2_it [t] 0 where
  sum_nodes2_it:: Num a => [Tree2 a] -> a -> a
  sum_nodes2_it [Leaf n] a = a + n
  sum_nodes2_it (Leaf n:ts) a = sum_nodes2_it ts (a+n)
  sum_nodes2_it (Node2 n t1 t2:ts) a = sum_nodes2_it (t1:t2:ts) (a+n)

-- Use the technique once more to rewrite inorder2 so it avoids doing any
-- concatenations, using only (:).
-- Hint 1: (:) produces lists from back to front, so you should do the same.
-- Hint 2: You may need to get creative with your lists of trees to get the
-- right output.
inorder2' :: Tree2 a -> [a]
inorder2' t = inorder2_it [t] [] where
  inorder2_it [Leaf n] a = n:a 
  inorder2_it (Leaf n:ts) a = inorder2_it ts (n:a)
  inorder2_it ((Node2 n t1 t2):ts) a = inorder2_it (t1:ts) (n:inorder2_it (t2:ts) a)

---- Part 4: Extra Credit ----------------

-- Convert a Tree into an equivalent Tree2, IF POSSIBLE. That is, given t1,
-- return t2 such that conv21 t2 = t1, if it exists. (In math, this is called
-- the "inverse image" of the function conv21.)  Thus, if conv21 t2 = t1, then
-- it should be that conv 12 t1 = Just t2. If there does not exist such a t2,
-- then conv12 t1 = Nothing. Do some examples on paper first so you can get a
-- sense of when this conversion is possible.
conv12 :: Tree a -> Maybe (Tree2 a)
conv12 Empty = Nothing
conv12 (Node x Empty Empty) = Just (Leaf x)
conv12 (Node x left right) = 
  case conv12 left of
    Nothing -> Nothing 
    Just left' -> case conv12 right of
                    Nothing -> Nothing
                    Just right' -> Just (Node2 x left' right')


-- Binary Search Trees. Determine, by making only ONE PASS through a tree,
-- whether or not it's a Binary Search Tree, which means that for every
-- Node a t1 t2 in the tree, every element in t1 is strictly less than a and
-- every element in t2 is strictly greater than a. Complete this for both
-- Tree a and Tree2 a.

-- Hint: use a helper function that keeps track of the range of allowable
-- element values as you descend through the tree. For this, use the following
-- extended integers, which add negative and positvie infintiies to Int:

data ExtInt = NegInf | Fin Int | PosInf deriving Eq

instance Show ExtInt where
  show NegInf     = "-oo"
  show (Fin n) = show n
  show PosInf     = "+oo"

instance Ord ExtInt where
  compare NegInf  NegInf  = EQ
  compare NegInf  _       = LT
  compare (Fin n) (Fin m) = compare n m
  compare (Fin n) PosInf  = LT
  compare PosInf  PosInf  = EQ
  compare _       _       = GT
  -- Note: defining compare automatically defines <, <=, >, >=, ==, /=

bst :: Tree Int -> Bool
bst = undefined
{- bst Empty = False
bst (Node xs Empty Empty) = True -}

bst2 :: Tree2 Int -> Bool
bst2 = undefined
{- bst2 Empty = False
bst2 (Node2 xs Empty Empty) = True
 -}

{- ** OUTPUT:

*Main> conv12 (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
Just (Node2 1 (Leaf 2) (Leaf 3))



 -}