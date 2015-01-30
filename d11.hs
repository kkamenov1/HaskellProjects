data Tree a = Empty | Node a (Tree a) (Tree a)

--Zadacha 1

isTherePath :: (Ord a) => a -> Tree a -> Bool
isTherePath _ Empty = False
isTherePath a (Node root left right)
	|a == root = True
	|otherwise = (isTherePath a left) || (isTherePath a right)

pathTo :: (Ord a) => a -> Tree a -> [a]
pathTo a Empty = error "Error"
pathTo a (Node root left right)
	|root == a = [root]
	|isTherePath a left = [root] ++ pathTo a left
	|isTherePath a right = [root] ++ pathTo a right
	
--Zadacha 3
level :: Int -> Tree a -> [a]
level 0 (Node root _ _) = [root]
level _ Empty = []
level n (Node root left right) = level (n-1) left ++ level (n-1) right

--Zadacha 2

isInTree :: (Ord a) => a -> Tree a -> Bool
isInTree _ Empty = False
isInTree a (Node root left right)
	|a == root = True
	|otherwise = isInTree a left || isInTree a right
	
lca :: (Ord a) => a -> a -> Tree a -> a
lca _ _ Empty = error "Error"
lca a b (Node root left right)
	|(isInTree a left && isInTree b right) || (isInTree a right) && (isInTree b left) = root
	|isInTree a left && isInTree b left = lca a b left
	|isInTree a right && isInTree b right = lca a b right