-- 1
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myLast' = last


-- 2

myButLast [] = error "empty"
myButLast [x] = error "only one"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

myButLast' xs = xs !! (length(xs) - 2)

-- 3

elemAt :: [a] -> Int -> Maybe a
elemAt [] _ = Nothing
elemAt (x:xs) n
	| n <  1 = error "postion must be >= 1"
	| n == 1 = Just x
	| n >  1 = elemAt xs (n-1)

-- 4
myLength :: [a] -> Int
myLength = len 0
	where
		len :: Int -> [a] -> Int
		len n [] = n
		len n (x:xs) = len (n+1) xs


-- 5
myReverse :: [a] -> [a]
myReverse = rev []
	where
		rev acc [] = acc
		rev acc (x:xs) = rev (x:acc) xs

myReverse' = foldl (flip (:)) []



-- 6
isPalindrome xs = xs == reverse xs

-- 7

data NestedList a = Elem a | List [NestedList a] deriving (Show,Eq)

flatten :: NestedList a -> [a]
flatten = doFlatten []
	where
		doFlatten xs (Elem x)		= xs ++ [x]
		doFlatten xs (List [])		= xs
		doFlatten xs (List (x:ys)) 	= doFlatten (doFlatten xs x) (List ys)


-- 8
-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = compress' [(head xs)] (head xs) xs
	where
		compress' :: Eq a => [a] -> a -> [a] -> [a]
		compress' acc _ [] 		=  reverse acc
		compress' acc c (x:xs)
			| c == x 	=  compress' acc	 x xs
			| otherwise =  compress' (x:acc) x xs

compress2 :: Eq a => [a] -> [a]
compress2 = comp2 []
	where
		comp2 acc []		= reverse acc
		comp2 acc [x]		= comp2 (x : acc) []
		comp2 acc (x:xs)
			| x == head xs	= comp2 acc xs
			| otherwise 	= comp2 (x:acc) xs



-- 9
-- Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack = pack' [] []
	where
		pack' :: Eq a => [[a]] -> [a] -> [a] -> [[a]]
		pack' acc acc' []  	= reverse acc
		pack' acc acc' [x]	= pack' ((x:acc'):acc) [] []
		pack' acc acc' (x:ys)
			| x == head ys	= pack' acc (x:acc') ys
			| otherwise		= pack' ((x:acc'):acc) [] ys


-- 10
-- run-length

encode :: Eq a => [a] -> [(Int, a)]
encode = fmap (\x -> (length x, head x)) . pack


