module BST where

data Tree a = Empty | Node (Tree a) a (Tree a)
  deriving Show

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node l v r)
  | x >  v = Node l v (insert x r)
  | x == v = Node l v r
  | x <  v = Node (insert x l) v r

data Nat = Z | S Nat
  deriving Show

instance Eq Nat where
  (==) Z Z = True
  (==) Z (S _) = False
  (==) (S _) Z = False
  (==) (S a) (S b) = a == b

instance Ord Nat where
  compare Z Z = EQ
  compare Z (S _) = LT
  compare (S _) Z = GT
  compare (S a) (S b) = compare a b
