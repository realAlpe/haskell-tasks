splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' 0 xs = ([], xs)
splitAt' n (x : xs) =
  let (ys, zs) = splitAt' (n - 1) xs
   in (x : ys, zs)

-- b) top-down
foldmTD :: (a -> a -> a) -> a -> [a] -> a
foldmTD _ e [] = e
foldmTD f e [x] = f e x
foldmTD f e xs =
  let (ys, zs) = splitAt (length xs `div` 2) xs
   in f (foldmTD f e ys) (foldmTD f e zs)

-- c) bottom-up
foldmBU :: (a -> a -> a) -> a -> [a] -> a
foldmBU _ e [] = e
foldmBU f e [x] = f e x
foldmBU f e (x : y : xs) =
  f (f x y) (foldmBU f e xs)
