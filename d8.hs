--Domashna 8
--Dimo Drangov 80936
import Data.Char
--Zadacha 1
encode :: String -> Int -> String
encode str n
	|str == [] = []
	|otherwise = [(chr ((ord (head str)) + n))] ++ encode (tail str) n
	
decode :: String -> Int -> String
decode str n = encode str (-n)

--Zadacha 2
lengthEncodeHelper :: String -> Int
lengthEncodeHelper [x] = 1
lengthEncodeHelper (x:xs)
	|x == (head xs) = 1 + lengthEncodeHelper xs
	|otherwise = 1

lengthEncode :: String -> String
lengthEncode [x] = [x]
lengthEncode str
	|str == [] = []
	|lengthEncodeHelper str == 1 = (head str):(lengthEncode (tail str))
	|otherwise = ((show (lengthEncodeHelper str)) ++ [head str]) ++ lengthEncode (drop (lengthEncodeHelper str) str)