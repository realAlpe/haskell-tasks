module Testing where

import Data.List (inits, tails)

maximumSegmentSum :: [Integer] -> Integer
maximumSegmentSum = maximum . map sum . segments

segments :: [a] -> [[a]]
segments = concat . map inits . tails

maxSegmentSum :: [Integer] -> Integer
maxSegmentSum = maximum . scanr (\a b -> max 0 (a + b)) 0

productExceptSelf :: [Int] -> [Int]
productExceptSelf nums =
  let prefix = scanl (*) 1 nums
      suffix = scanr (*) 1 nums
  in zipWith (*) prefix (tail suffix)

 