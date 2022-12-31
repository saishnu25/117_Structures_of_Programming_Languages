module Queue2 (Queue, mtq, ismt, addq, remq) where

---- Interface ----------------
mtq  :: Queue a                  -- empty queue
ismt :: Queue a -> Bool          -- is the queue empty?
addq :: a -> Queue a -> Queue a  -- add element to front of queue
remq :: Queue a -> (a, Queue a)  -- remove element from back of queue;
                                 --   produces error "Can't remove an element
                                 --   from an empty queue" on empty

--- Implementation -----------

{- In this implementation, a queue is represented as a pair of lists.
The "front" of the queue is at the head of the first list, and the
"back" of the queue is at the HEAD of the second list.  When the
second list is empty and we want to remove an element, we REVERSE the
elements in the first list and move them to the back, leaving the
first list empty. We can now process the removal request in the usual way.
-}

data Queue a = Queue2 [a] [a]  -- deriving Show

mtq = Queue2 [][]

ismt (Queue2 [][])= True
ismt (Queue2 _ _)= False

addq x (Queue2 xs ys) = Queue2 xs (x:ys)

remq (Queue2 [] []) = error "Queue is empty"
remq (Queue2 [] ys) = remq (Queue2 (reverse ys) [])
remq (Queue2 (x:xs) (ys)) = (x, Queue2 xs ys)


