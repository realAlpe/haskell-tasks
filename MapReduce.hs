-- b) top-down
foldmTD :: (a -> a -> a) -> a -> [a] -> a
foldmTD _ e [] = e
foldmTD f e [x] = f e x
foldmTD f e xs = f (foldmTD f e left) (foldmTD f e right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

-- c) bottom-up
foldmBU :: (a -> a -> a) -> a -> [a] -> a
foldmBU _ e [] = e
foldmBU _ _ [x] = x
foldmBU f e xs = foldmBU f e $ step xs
  where
    step [] = []
    step [x] = [x]
    step (x : y : ys) = f x y : step ys
