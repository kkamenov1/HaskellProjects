--Domashna 10
--Dimo Drangov 80936

--Zadacha 1
positionH :: [String] -> Int -> Int ->[(Int,Int)]
positionH str row col
	|str == [] = []
	|head str == "-" = [(row,col)] ++ positionH (tail str) row (col + 1)
	|otherwise = positionH (tail str) row (col + 1)
	
position :: [String] -> Int -> [(Int,Int)]
position str row = positionH str row 0

positions :: [[String]] -> [(Int, Int)]
positions matrix = position (head matrix) 0 ++ position (head (tail matrix)) 1 ++ position (head (tail (tail matrix))) 2

replaceIndexAtRowH :: [String] -> Int -> String -> [String]
replaceIndexAtRowH str n strToReplace
	|str == [] = []
	|n == 0 = [strToReplace] ++ tail str
	|otherwise = [(head str)] ++ replaceIndexAtRowH (tail str) (n -1) strToReplace
	
replaceAtIndex :: [[String]] -> (Int,Int) -> String -> [[String]]
replaceAtIndex str (row,col) strToReplace
	|str == [] = []
	|row == 0 = [(replaceIndexAtRowH (head str) col strToReplace)] ++ (tail str)
	|otherwise = [head str] ++ replaceAtIndex (tail str) (row - 1,col) strToReplace
	
nextStatesH :: [[String]] -> String -> [(Int,Int)] -> [[[String]]]
nextStatesH matrix strToReplace indexes
	|indexes == [] = []
	|otherwise = [(replaceAtIndex matrix (head indexes) strToReplace)] ++ nextStatesH matrix strToReplace (tail indexes)

nextStates :: [[String]] -> String -> [[[String]]]
nextStates matrix player = nextStatesH matrix player (positions matrix)	

--Zadacha 2

isInList ::(Eq a) => [a] -> a -> Bool
isInList list a
	|list == [] = False
	|(head list) == a = True
	|otherwise = isInList (tail list) a

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection list1 list2
	|list1 == [] = []
	|isInList list2 (head list1) = [(head list1)] ++ intersection (tail list1) list2
	|otherwise = intersection (tail list1) list2
	
--Zadacha 3
difference :: (Eq a) => [a] -> [a] -> [a]
difference list1 list2 
	|list1 == [] = []
	|not(isInList list2 (head list1)) = [(head list1)] ++  difference (tail list1) list2
	|otherwise = difference (tail list1) list2