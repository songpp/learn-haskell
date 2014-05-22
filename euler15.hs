module Main where

import Data.Array

routes :: (Int,Int) -> Int
routes = (rte !)
  where rte = array ((0,0), (n,n))
                    [ ((x,y), route (x,y)) | x <- [0 .. n],
                                             y <- [0 .. n] ]
        route (0,0) = 0
        route (_,0) = 1
        route (0,_) = 1
        route (x,y)
          | x < 0 || y < 0 = 0
          | otherwise      = rte ! (x-1,y) + rte ! (x,y-1)
        n = 20
