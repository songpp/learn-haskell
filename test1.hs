{-#LANGUAGE GADTs#-}
module Main where

import System.IO
import Network.HTTP
import Data.Char(toUpper)

main :: IO ()
main = print "Hello World"

fetchUrl url = simpleHTTP (getRequest url)

allLines file = do
	h <- openFile file ReadMode
	mainloop h
	hClose h

mainloop :: Handle -> IO ()
mainloop inh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   putStrLn inpStr
                   mainloop inh

allLines2 :: FilePath -> IO ()
allLines2 file = openFile file ReadMode >>= \h -> mainloop h >> hClose h

data Expr f = In (f  (Expr f))

data Val e = Val Int deriving (Show)

type IntExpr = Expr Val

one :: IntExpr
one = In (Val 1)


myg n | n > 0 = 1
	  | n < 0 = 2
	  
threeXplus1 :: (Num a) => a -> Int
threeXplus1 n
	| n == 1 = 1 --putStrLn "1, over" 
	| n < 0 = 0 --putStrLn "0, error" 
	| n `mod` 2 == 0 = return n >>= \n -> putStrLn (show n) >> threeXplus1 (n `div` 2)
	| otherwise = return n >>= \n -> putStrLn (show n) >> threeXplus1 (3 * n + 1)
