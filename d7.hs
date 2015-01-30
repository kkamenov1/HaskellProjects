drop' :: Int -> [a] -> [a]
drop' n list
	|n==0 = list
	|otherwise = (drop' (n-1) (tail list))