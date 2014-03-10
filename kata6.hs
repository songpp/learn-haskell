module Anagrams where

import           Data.Char          (ord, toLower)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (sortBy)
import           System.Environment
import           Text.Printf        (printf)

main = do
    (filename : _) <- getArgs
    contents <- readFile filename
    let result = analyze . lines . map toLower  $ contents
    let r = take 10 . sortBy sortA $ IntMap.elems result
    print r

type Words = [String]
data A = A { size :: !Int, ws :: Words }
instance Show A where
    show (A size words) = printf "%d => %s\n" size (show words)

sortA a1 a2 =  size a2 `compare` size a1

analyze :: Words -> IntMap A
analyze = foldl insert IntMap.empty . map wordHash
    where
        wordHash w = (hashWord w,  w)
        insert :: IntMap A -> (Int, String) -> IntMap A
        insert acc (hash, word) = IntMap.insertWith increment hash (A 1 [word]) acc
        increment (A s1 w1) (A s2 w2) = A (s1 + s2) (w1 ++ w2)


hashWord :: String -> Int
hashWord = product . map factors
    where
        factors = (hashFactors IntMap.!) . ord


hashFactors = IntMap.fromList $ zip letters primes
    where
        letters = map ord (['a'..'z'] ++ "'-" ++ ['0'..'9'])
        primes = [ x | x <- [2 .. ], isPrime x]
        isPrime n = not $ any (\t -> n `rem` t == 0)
                        [2 ..  floor . sqrt . fromIntegral $ n]
