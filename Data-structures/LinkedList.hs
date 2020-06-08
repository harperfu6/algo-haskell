module LinkedList (
    LinkedList
) where

data LinkedList a = Nil | Cell a (LinkedList a) deriving Show