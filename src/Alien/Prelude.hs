{-# LANGUAGE RebindableSyntax, NegativeLiterals, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-name-shadowing -Wno-missing-signatures #-}

module Alien.Prelude
  ( add
  , mul
  , div
  , eq
  , lt
  , neg
  , s
  , c
  , b
  , t
  , f
  , i
  , cons
  , car
  , cdr
  , nil
  , isnil
  ) where

import qualified Unsafe.Coerce as H (unsafeCoerce)
import qualified Prelude as H ((+), (*), negate, quot, (==), (<), Integer, Bool(..))

-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message7.png>>
add = (H.+) :: H.Integer -> H.Integer -> H.Integer
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message9.png>>
mul = (H.*) :: H.Integer -> H.Integer -> H.Integer
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message10.png>>
div = H.quot :: H.Integer -> H.Integer -> H.Integer
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message11.png>>
eq = \x y -> case (x :: H.Integer) H.== y of H.True -> t; H.False -> f
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message12.png>>
lt = \x y -> case (x :: H.Integer) H.< y of H.True -> t; H.False -> f
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message16.png>>
neg = H.negate :: H.Integer -> H.Integer
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message18.png>>
s = H.unsafeCoerce (\f g x -> f x (g x))
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message19.png>>
c = \f x y -> f y x
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message20.png>>
b = \f g x -> f (g x)
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message21.png>>
t = \t f -> t
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message22.png>>
f = \t f -> f
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message24.png>>
i = \x -> x
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message25.png>>
cons = H.unsafeCoerce (\x y p -> p x y)
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message26.png>>
car = \p -> p t
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message27.png>>
cdr = \p -> p f
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message28.png>>
nil = H.unsafeCoerce (\x -> t)
-- | <<https://message-from-space.readthedocs.io/en/latest/_images/message29.png>>
--
-- /Translation note: a conforming implementation is @isnil x = x (t (t f))@ or @isnil = c i (t (t f))@./
isnil = \x -> x (t (t f))
