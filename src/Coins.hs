module Coins where

import Data.List (permutations)

solution :: (Int, Int, Int, Int, Int)
solution = head [ (a, b, c, d, e) | [a, b, c, d, e] <- permutations [2,3,5,7,9], f a b c d e == 399 ]
  where f a b c d e = a + b * c * c + d * d * d - e
