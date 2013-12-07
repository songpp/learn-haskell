

import           System.Random

-- 21
-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]

insertAt a xs n = insert' [] 0 xs
	where
		insert' acc idx [] = acc ++ [a]
		insert' acc idx	t@(x:xs)
			| idx == n = acc ++ (a:t)
			| idx <  n = insert' (acc ++ [x]) (idx+1) xs


insertAt2 a xs n = l ++ (a : r)
	where
		(l,r) = splitAt n xs


-- 22
-- Create a list containing all integers within a given range.

-- range start end
range :: Int -> Int -> [Int]
range i n = range' [] i
	where
		range' acc i
			| i >  n = reverse acc
			| i <= n = range' (i:acc) (i+1)


-- 23 random
-- 24
-- 25

-- 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations 1 xs = xs
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)



