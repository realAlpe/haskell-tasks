module Calculus where

infixl 6 :+:

infixl 7 :*:

infixl 8 :^:

infixl 9 :.:

data Function
  = Const Rational -- constant function
  | Id -- identity
  | Function :+: Function -- addition of functions
  | Function :*: Function -- multiplication of functions
  | Function :^: Integer -- power
  | Function :.: Function -- composition of functions
  deriving (Show)

--------------------------------------------------------------------------------
-- a)
apply :: Function -> (Rational -> Rational)
apply (Const c) = const c
apply Id = id
apply (f :+: g) = \x -> apply f x + apply g x
apply (f :*: g) = \x -> apply f x * apply g x
apply (f :^: n) = \x -> apply f x ^^ n
apply (f :.: g) = apply f . apply g

--------------------------------------------------------------------------------
-- b)
derive :: Function -> Function
derive (Const _) = Const 0
derive Id = Const 1
derive (f :+: g) = derive f :+: derive g
derive (f :*: g) = (derive f :*: g) :+: (f :*: derive g)
derive (f :^: n) = Const (toRational n) :*: (f :^: (n - 1)) :*: derive f
derive (f :.: g) = (derive f :.: g) :*: derive g

--------------------------------------------------------------------------------
-- c)
simplify :: Function -> Function
-- constant
simplify (Const x) = Const x
-- identity
simplify Id = Id
-- addition
simplify (Const 0 :+: g) = simplify g
simplify (f :+: Const 0) = simplify f
simplify (f :+: g) = simplify (simplify f :+: simplify g)
-- multiplication
simplify (Const 0 :*: _) = Const 0
simplify (_ :*: Const 0) = Const 0
simplify (Const 1 :*: g) = simplify g
simplify (f :*: Const 1) = simplify f
simplify (f :*: g) = simplify (simplify f :*: simplify g)
-- exponentiation
simplify (Const 0 :^: _) = Const 0
simplify (Const 1 :^: _) = Const 1
simplify (_ :^: 0) = Const 1
simplify (f :^: 1) = simplify f
simplify (f :^: n) = simplify (simplify f :^: n)
-- concatenation
simplify (Const c :.: _) = Const c
simplify (f :.: Const c) = Const (apply f c)
simplify (Id :.: g) = simplify g
simplify (f :.: Id) = simplify f
simplify (f :.: g) = simplify (simplify f :.: simplify g)
