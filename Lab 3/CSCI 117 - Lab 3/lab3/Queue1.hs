module Queue1 (Queue, mtq, ismt, addq, remq) where

---- Interface ----------------
mtq  :: Queue a                  -- empty queue
ismt :: Queue a -> Bool          -- is the queue empty?
addq :: a -> Queue a -> Queue a  -- add element to front of queue
remq :: Queue a -> (a, Queue a)  -- remove element from back of queue;
                                 --   produces error "Can't remove an element
                                 --   from an empty queue" on empty

---- Implementation -----------

{- In this implementation, a queue is represented as an ordinary list.
The "front" of the queue is at the head of the list, and the "back" of
the queue is at the end of the list.
-}

data Queue a = Queue1 [a] -- deriving Show

--Empty queue
mtq = Queue1 []

-- Check if queue is empty
ismt (Queue1 xs) = length xs == 0

-- Adding elements to the front of the queue
addq x (Queue1 xs) = Queue1 (x:xs)

-- Removing element from the front, FIFO (First In, First Out)
remq (Queue1 xs) = if(length xs == 0)

                    then error "Queue is empty"

                    else (last xs, Queue1 (init xs))