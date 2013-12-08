-- 11
-- 9
-- Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack = pack' [] []
	where
		pack' :: Eq a => [[a]] -> [a] -> [a] -> [[a]]
		pack' acc acc' []  	= reverse acc
		pack' acc acc' [x]	= pack' ((x:acc'):acc) [] []
		pack' acc acc' (x:ys)
			| x == head ys = pack' acc (x:acc') ys
			| otherwise = pack' ((x:acc'):acc) [] ys

data RunLength a = Single a | Multi Int a deriving Show

encodeModified :: Eq a => [a] -> [RunLength a]
encodeModified = fmap rl . pack
	where
		rl :: [a] -> RunLength a
		rl xs = if length xs > 1
				then Multi (length xs) (head xs)
				else Single (head xs)


-- 12
-- decode run-length encoded list
-- decode [Multi 4 'a',Single 'd',Single 'f',Single 's',Single 'd',Multi 5 'f']

decode :: [RunLength a] -> [a]
decode = dec' []
	where
		dec' acc [] 	= reverse acc
		dec' acc (x:xs) = dec' (check acc x) xs
		check :: [a] -> RunLength a -> [a]
		check acc (Single s) 	= s : acc
		check acc (Multi 0 s)	= acc
		check acc (Multi n s) 	= check (s:acc) (Multi (n-1) s)



-- 13
-- (**) Run-length encoding of a list (direct solution).

runlen :: Eq a => [a] -> [RunLength a]
runlen [] = []
runlen (x:xs) = run' [] (1,x) xs
	where
--		run' :: Eq a => [a] -> (Int, a) -> [a] -> []
		run' acc c [] 		= fmap convert . reverse $ acc
		run' acc (n,v) [x]	= run' ((n+1, v): acc) (0,x) []
		run' acc c@(n,v) (x:xs)
			| v == x 		= run' acc (n+1, v) xs
			| otherwise = run' (c:acc) (1, x) xs

		convert (n, x) = if n > 1
							then Multi n x
							else Single x


-- 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs


-- 15

repli :: [a] -> Int -> [a]
repli [] 	 _= []
repli (x:xs) n 	= (loop x n []) ++ repli xs n
	where
		loop x 0 acc = acc
		loop x n acc = loop x (n-1) (x:acc)


dupli2 = flip repli 2


-- 16
-- (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery = dropEvery' 1 []
	where
		dropEvery' c acc [] 		n 	= reverse acc
		dropEvery' c acc (x:xs) 	n
			| c `rem` n == 0 = dropEvery' (c+1) acc 	  xs n
			| otherwise      = dropEvery' (c+1) (x:acc) xs n


-- 17
-- (*) Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> ([a], [a])
-- split xs 0 = ([],xs)
split = split' []
	where
		split' xs [] n = (reverse xs, [])
		split' xs  ys  0 = (reverse xs, ys)
		split' xs (x:ys) n = split' (x:xs) ys (n-1)


-- 18
-- slice
slice :: [a] -> Int -> Int -> [a]
slice xs start end
	| start < 0  	= error "start must be >= 0"
	| start > end 	= error "start must be <= `end`"
	| otherwise= slice' xs [] 1
	where
		slice' []     acc _ = reverse acc
		slice' (x:xs) acc c
			| c >= start && c <= end = slice' xs (x:acc) (c+1)
			| otherwise = slice' xs acc (c+1)


-- 19
-- rotate list

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n
	| n > 0= snd positiveR ++ fst positiveR
	| n < 0 = snd negtiveR ++ fst negtiveR
	where
		positiveR = split xs n
		negtiveR = split xs (length xs + n)


-- 20
-- (*) Remove the K'th element from a list.

removeAt = removeAt' (1,[])
	where
		removeAt' (idx,ys) 0 xs = xs
	 	removeAt' (idx,ys) n [] = reverse ys
	 	removeAt' (idx,ys) n l@(x:xs)
	 		| n > length l = l
	 		| idx == n = reverse ys ++ xs
	 		| idx <  n = removeAt' (idx + 1, x:ys) n xs








