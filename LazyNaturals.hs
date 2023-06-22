infixr 6 :+

data Super natural = O | !natural :+ Super natural
  deriving (Show)

-- a)
value :: Num natural => Super natural -> natural
value O = 0
value (p :+ ps) = p + value ps

-- b)
width :: Num natural => Super natural -> natural
width O = 0
width (_ :+ ps) = 1 + width ps

height :: (Num natural, Ord natural) => Super natural -> natural
height O = 0
height (p :+ ps) = max p (height ps)

-- c)
-- We instantiate the Num type class such that we can use the symbols + and *

instance (Num natural, Ord natural) => Num (Super natural) where
  (+) :: (Num natural, Ord natural) => Super natural -> Super natural -> Super natural
  ps + O = ps
  O + qs = qs
  (a :+ as) + (b :+ bs) =
    (a + b) :+ (as + bs)

  (*) :: (Num natural, Ord natural) => Super natural -> Super natural -> Super natural
  _ * O = O
  O * _ = O
  (a :+ as) * (b :+ bs) =
    (a * b) :+ ((a :+ O) * bs + (b :+ O) * as + as * bs)

  -- the following functions are required for the Num type class, don't worry too much
  negate _ = O
  abs n = n
  signum O = O
  signum (_ :+ _) = 1
  fromInteger n = if n == 0 then O else fromInteger n :+ O

-- d)
instance (Num natural, Ord natural) => Ord (Super natural) where
  compare :: (Num natural, Ord natural) => Super natural -> Super natural -> Ordering
  compare O O = EQ
  compare O _ = LT
  compare _ O = GT
  compare p q = compare (value p) (value q) -- bad since everything is evaluated (not too lazy)

-- compare (a :+ as) (b :+ bs) = value

-- Ord requires that Eq is defined as well. So we define Eq using Ord.
instance (Num natural, Ord natural) => Eq (Super natural) where
  m == n = compare m n == EQ

k1 :: Super Integer
k1 = 1 :+ 3 :+ 5 :+ 1 :+ 4 :+ 2 :+ 1 :+ O -- 17

k2 :: Super Integer
k2 = 9 :+ 12 :+ O -- 21