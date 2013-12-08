
{-# LANGUAGE LambdaCase #-}
import           Data.Maybe
import           Prelude    hiding (gcd)
-- 31
-- isPrime

isPrime n = null allRems
    where
        allRems = [x | x <- [2 .. (n - 1)], n `rem` x == 0]


-- 32
-- GCD
-- euclid

gcd a 0 = a
gcd a b = gcd b (a `rem` b)



-- 33
-- Determine whether two positive integer numbers are coprime.
coprime a b = 1 == gcd a b


-- 34
-- Calculate Euler's totient function phi(m).

phi n = length [x | x <- [1 .. (n - 1)], coprime n x]


-- 35
-- prime factors

primes n = [x| x <- [2 .. n], isPrime x]

primeFactors :: Integral a => a -> [(a, a)]
primeFactors n = pf [] (primes n) n
    where
        pf acc fs 0 = reverse acc
        pf acc [] n = reverse (prependAcc n acc)
        pf acc fs@(x:xs) n
            | n <= x               =  pf acc [] n
            | n `rem` x == 0      =  pf (prependAcc x acc) fs (n `div` x)
            | otherwise              =  pf acc xs n

        prependAcc a acc@((x,n):xs)
            | a == x     = (x,n+1):xs
            | otherwise = (a,1):acc
        prependAcc a xs = (a, 1):xs




-- 36
-- as 35

-- 37

phi2 :: Integer -> Double
phi2 m = foldl calc 1.0 (primeFactors m)

calc :: Double -> (Integer, Integer) -> Double
calc acc (p, m) = acc * (fromIntegral (p-1) * fromIntegral p ** fromIntegral (m - 1))


-- 39
primesR i n = filter isPrime [i .. n]

first f [] = Nothing
first f (x:xs)
    | f x = Just x
    | otherwise = first f xs


-- 40
goldbach n = first (isJust . snd) (loop primes) >>= \(a, Just b) -> return (a,b)
    where
        loop [x] = [(x, Nothing)]
        loop (x:xs) = (x, find x (reverse xs)) : loop xs
        find a xs = first (\b -> a + b == n) xs
        primes = primesR 1 n


goldbach2 n =  map (\(a, Just b) -> (a,b)) . filter (isJust . snd) $ (loop primes)
    where
        loop [x] = [(x, Nothing)]
        loop (x:xs) = (x, find x (reverse xs)) : loop xs
        find a xs = first (\b -> a + b == n) xs
        primes = primesR 1 n


-- 41
evens i n = filter even $ [i..n]

data Goldbach =
    G Integer Integer Integer | G2 Integer [(Integer, Integer)]

instance Show Goldbach where
    show (G x a b) = show x ++ " = " ++ show a ++ " + " ++ show b
    show (G2 x xs) = "\n" ++ show x ++ " = " ++ show xs

goldbachList i n v = find (evens i n)
    where
        find = filter (\(G x a b) -> a >= v && b >= v)
             . map (\x -> let Just (a,b) = goldbach x in G x a b)


goldbachList2 i n v = filter (not . emptyG2) . map (find . generate) $ (evens i n)
    where
        emptyG2 (G2 x xs) = null xs
        find (G2 x xs) = G2 x (avaliablePairs xs)
        avaliablePairs = filter (\(a, b) -> a >= v && b >= v)
        generate n = G2 n (goldbach2 n)





