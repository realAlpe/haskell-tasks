data Bit = O | I
  deriving (Show, Eq, Ord)

newtype Binary = Bin {fromBinary :: [Bit]}

instance Eq Binary where
  (==) :: Binary -> Binary -> Bool
  Bin a == Bin b = reduce a == reduce b

removeLeading :: [Bit] -> [Bit]
removeLeading [] = []
removeLeading (O : xs) = removeLeading xs
removeLeading xs = xs

-- remove trailing O
reduce :: [Bit] -> [Bit]
reduce = reverse . removeLeading . reverse
