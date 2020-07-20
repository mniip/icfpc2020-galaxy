{-# LANGUAGE RebindableSyntax, NegativeLiterals, NoImplicitPrelude #-}

module Alien.Prelude
  ( s
  , b
  , t
  , f
  , c
  , i
  , cons
  , nil
  , isnil
  , car
  , cdr
  , lt
  , eq
  , mul
  , div
  , neg
  , add
  ) where

import qualified Unsafe.Coerce as H (unsafeCoerce)
import qualified Prelude as H ((+), (*), negate, quot, (==), (<), Integer, Bool(..))

s = H.unsafeCoerce (\f g x -> f x (g x))
b = \f g x -> f (g x)
cons = H.unsafeCoerce (\x y p -> p x y)
car = \p -> p t
cdr = \p -> p f
t = \t f -> t
f = \t f -> f
c = \f x y -> f y x
nil = H.unsafeCoerce (\x -> t)
i = \x -> x
isnil = \x -> x (t (t f)) -- my own

lt = \x y -> case (x :: H.Integer) H.< y of H.True -> t; H.False -> f
eq = \x y -> case (x :: H.Integer) H.== y of H.True -> t; H.False -> f
mul = (H.*) :: H.Integer -> H.Integer -> H.Integer
div = H.quot :: H.Integer -> H.Integer -> H.Integer
neg = H.negate :: H.Integer -> H.Integer
add = (H.+) :: H.Integer -> H.Integer -> H.Integer

