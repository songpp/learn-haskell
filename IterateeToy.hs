module IterateeToy where

import Data.Maybe
import Control.Monad

-- Iteratee toy

type LC = Maybe Char

data I a = Done a
         | Cont (LC -> I a)

getline :: I String 
getline = loop ""
  where loop acc = Cont (check acc)
        check acc (Just c) | c /= '\n' = loop (c : acc)
        check acc _                    = Done (reverse acc)

getlines :: I [String]
getlines = loop []
  where loop acc = getline >>= check acc
        check acc "" = return (reverse acc)
        check acc l  = loop (l : acc)

eval ::  String -> I a -> a
eval ""     (Cont k) = eval "" $ k Nothing
eval (c:t)  (Cont k) = eval t  $ k (Just c)
eval str    (Done x) = x

instance Monad I where
  return = Done

  Done x >>= f = f x
  -- k >=> f  is   \x -> k x >>= f
  Cont k >>= f = Cont (k >=> f)

enstr :: String -> I a -> I a
enstr ""    i        = i
enstr (c:t) (Cont k) = enstr t $ k (Just c)
enstr _     (Done x) = Done x

run :: I a -> a
run (Done x)  = x
run (Cont k)  = run $ k Nothing
