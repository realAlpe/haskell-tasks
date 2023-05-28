data Bit = O | I
  deriving (Show, Eq, Ord)

class Data a where
  encode :: (a, [Bit]) -> [Bit]
  decode :: [Bit] -> (a, [Bit])

serialize :: (Data a) => a -> [Bit]
serialize a = encode (a, [])

deserialize :: (Data a) => [Bit] -> a
deserialize xs = let (a, []) = decode xs in a

instance Data Bool where
  -- O -> False, I -> True
  encode :: (Bool, [Bit]) -> [Bit]
  encode (True, xs) = I : xs
  encode (False, xs) = O : xs

  decode :: [Bit] -> (Bool, [Bit])
  decode [] = error "Input cannot be empty."
  decode (I : xs) = (True, xs)
  decode (O : xs) = (False, xs)

instance Data Bit where
  encode :: (Bit, [Bit]) -> [Bit]
  encode (x, xs) = x : xs

  decode :: [Bit] -> (Bit, [Bit])
  decode [] = error "Input cannot be empty."
  decode xs = (head xs, tail xs)

instance Data () where
  encode :: ((), [Bit]) -> [Bit]
  encode ((), xs) = xs

  decode :: [Bit] -> ((), [Bit])
  decode xs = ((), xs)

instance (Data a) => Data (Maybe a) where
  -- O -> Nothing, I -> Just a
  encode :: Data a => (Maybe a, [Bit]) -> [Bit]
  encode (Nothing, xs) = O : xs
  encode (Just a, xs) = I : encode (a, xs)

  decode :: Data a => [Bit] -> (Maybe a, [Bit])
  decode [] = error "Input cannot be empty."
  decode (O : xs) = (Nothing, xs)
  decode (I : xs) = let (a, ys) = decode xs in (Just a, ys)

instance (Data a) => Data [a] where
  -- O -> empty list, I -> non-empty list
  encode :: Data a => ([a], [Bit]) -> [Bit]
  encode ([], xs) = O : xs
  encode (x : xs, ys) = I : encode (x, encode (xs, ys))

  decode :: Data a => [Bit] -> ([a], [Bit])
  decode (O : xs) = ([], xs)
  decode (I : xs) =
    let (x, zs) = decode xs
        (ys, ws) = decode zs
     in (x : ys, ws)
  decode _ = error "Input cannot be empty."

instance (Data a, Data b) => Data (a, b) where
  encode :: (Data a, Data b) => ((a, b), [Bit]) -> [Bit]
  encode ((a, b), xs) = encode (a, []) ++ encode (b, xs)

  decode :: (Data a, Data b) => [Bit] -> ((a, b), [Bit])
  decode xs =
    let (a, ys) = decode xs :: (a, [Bit])
        (b, zs) = decode ys :: (b, [Bit])
     in ((a, b), zs)

integerToBits :: Integer -> [Bit]
integerToBits 0 = []
integerToBits x =
  let (q, r) = quotRem x 2
   in (if r == 0 then O else I) : integerToBits q

bitsToInteger :: [Bit] -> Integer
bitsToInteger [] = 0
bitsToInteger (x : xs) = (if x == O then 0 else 1) + 2 * bitsToInteger xs

instance Data Integer where
  -- O -> non-negative, I -> negative
  encode :: (Integer, [Bit]) -> [Bit]
  encode (a, xs) =
    (if a < 0 then I else O) : encode (integerToBits (abs a), xs) -- encode with [Bit]

  decode :: [Bit] -> (Integer, [Bit])
  decode (x : xs) =
    let (ys, zs) = decode xs :: ([Bit], [Bit]) -- decode xs with [Bit], isomorphic to Integer!
     in ((if x == O then 1 else -1) * bitsToInteger ys, zs)
  decode _ = error "Input cannot be empty."

encDec :: (Data a) => (a, [Bit]) -> (a, [Bit])
encDec = decode . encode

serDes :: (Data a) => a -> a
serDes = deserialize . serialize