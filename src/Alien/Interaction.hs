{-# LANGUAGE LambdaCase #-}
module Alien.Interaction where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.List
import Data.Maybe

import Alien.FFI

-- | Encode a structure as a squiggly
modulate :: IntList -> [Bool]
modulate (LInt n)     = (if n >= 0 then [False, True] else [True, False])
                        ++ replicate len True
                        ++ [False]
                        ++ map (testBit absn) (reverse [0 .. 4*len-1])
                        where
                          absn = abs n
                          len = head $ dropWhile (\l -> 16^l <= absn) [0..]
modulate LNil         = [False, False]
modulate (LCons x xs) = [True, True] ++ modulate x ++ modulate xs

-- | Decode a squiggly into a structure
demodulate :: [Bool] -> IntList
demodulate = fromMaybe (error "Demodulate parse error") . evalStateT (go <* end)
  where
    getBit = StateT uncons
    end = StateT $ \case
      [] -> pure ((), [])
      _  -> empty
    go = liftA2 (,) getBit getBit >>= \case
      (False, False) -> pure LNil
      (True, True) -> LCons <$> go <*> go
      (sign, _) -> LInt <$> do
        let getLen = getBit >>= \b -> if b then succ <$> getLen else pure 0
        len <- getLen
        mantissa <- replicateM (4 * len) getBit
        pure $ (if sign then negate else id) $ foldl' (\x y -> 2*x + if y then 1 else 0) 0 mantissa

-- | Loop the interaction until it demands a "click".
makeClick
  :: Monad m
  => (IntList -> m IntList) -- ^ Callback to contact the alien ship
  -> (AlienState -> m ()) -- ^ Executed on every change of state
  -> alienValue
  -> AlienState
  -> (Integer, Integer) -- ^ Click coordinates
  -> m (AlienState, [Drawing])
makeClick send step interactor stt (x, y) = go stt (LCons (LInt x) (LInt y))
  where
    go st input = step st >> case interactWith interactor st input of
      (st', Left request) -> go st' =<< send request
      (st', Right pictures) -> pure (st', pictures)

-- | Loop the interaction.
runInteraction
  :: Monad m
  => (IntList -> m IntList) -- ^ Callback to contact the alien ship
  -> ([Drawing] -> m (Integer, Integer)) -- ^ Callback for displaying a list of 'Drawing's and awaiting for a "click"
  -> (AlienState -> m ()) -- ^ Executed on every change of state
  -> alienValue
  -> AlienState -- ^ Initial state
  -> m ()
runInteraction send click step interactor initState = click [] >>= goClick initState
  where
    goClick st (x, y) = go st (LCons (LInt x) (LInt y))
    go st input = step st >> case interactWith interactor st input of
      (st', Left request) -> send request >>= go st'
      (st', Right pictures) -> click pictures >>= goClick st'
