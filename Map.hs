module Map where

import Prelude hiding (lookup)

type Map a = String -> Maybe a

empty :: Map a
empty _ = Nothing

insert :: (String, a) -> Map a -> Map a
insert (s, a) m s2
  | s == s2 = Just a
  | otherwise = lookup s2 m

lookup :: String -> Map a -> Maybe a
lookup x m = m x

fromList :: [(String, a)] -> Map a
fromList [] _ = Nothing
fromList (x : xs) s
  | fst x == s = Just (snd x)
  | otherwise = fromList xs s

-- Alternatively
fromList2 :: [(String, a)] -> Map a
fromList2 = foldr insert empty

-- more general map2
type Map2 a b = a -> Maybe b

empty2 :: Map2 a b
empty2 _ = Nothing

insert2 :: (Eq a) => (a, b) -> Map2 a b -> Map2 a b
insert2 (k1, v) m k2
  | k1 == k2 = Just v
  | otherwise = lookup2 k2 m

lookup2 :: a -> Map2 a b -> Maybe b
lookup2 v m = m v