module Triangle where

type Triangle = (Integer, Integer, Integer)

height :: Triangle -> Integer
height (a, b, c) = abs (a + b + c) -- height (a, 0, 0) + height (0, b, 0)+ height (0, 0, c)
