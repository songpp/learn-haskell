{-# LANGUAGE BangPatterns #-}
module Main where


{-
ghc --make -O3 erler14.hs -o collatz
time ./colltz

real  0m2.701s
user  0m2.683s
sys 0m0.011s
-}
main :: IO ()
main = do
  let l = calc3 1000000
  print l


calc3 :: Int -> (Int, Int)
calc3 m = start 1 (1,1)
  where
    start x (n,t)
      | x > m = (n,t)
      | otherwise = let terms = collatz x
                    in if terms > t
                        then start (x+1) (x, terms)
                        else start (x+1) (n, t)

collatz :: Int -> Int
collatz = flip collatz' 1

collatz' :: Int -> Int -> Int
collatz' 1 acc = acc
collatz' c acc | even c    = collatz' (c `div` 2) (acc + 1)
               | otherwise = collatz' (3 * c + 1) (acc + 1)
