{-# LANGUAGE MagicHash, UnboxedTuples, LambdaCase #-}
module Alien.FFI where

import GHC.Prim
import GHC.Word
import Unsafe.Coerce

import qualified Alien.Prelude as A

-- | The aliens have clearly ascended past any need for types, using, perhaps
-- some technique like "not making any mistakes in your code". 
-- To them, @b (s i i) (c b (s i i))@ is perfectly valid. To us this is
-- incomprehensible gibberish. It is for this reason that we shall ask the
-- question which no human shall dare ask:
--
-- Is this value a data constructor, or a function closure?
isData :: alienValue -> Bool
isData x = x `seq` case unpackClosure# x of
  (# infoTable, _, _ #) ->
    let closType = W# (indexWord32OffAddr# infoTable 2#) in
      (closType > 0 && closType < 8) {- ClosureTypes.h: 1-7 are CONSTR_* -}

-- | A squiggle-encodeable structure
data IntList
  = LInt !Integer
  | LCons !IntList !IntList
  | LNil
  deriving (Eq, Ord, Show, Read)

-- | Extract an 'IntList' from the alien dimension into ours.
extractIntList :: alienValue -> IntList
extractIntList x = if isData x
  then LInt $ unsafeCoerce x -- hopefully an Integer
  else unsafeCoerce A.isnil x LNil $ LCons (extractIntList $ unsafeCoerce A.car x) (extractIntList $ unsafeCoerce A.cdr x)

-- | Inject an 'IntList' into the alien dimension.
injectIntList :: IntList -> alienValue
injectIntList LNil = unsafeCoerce A.nil
injectIntList (LCons car cdr) = unsafeCoerce A.cons (injectIntList car) (injectIntList cdr)
injectIntList (LInt int) = unsafeCoerce int

newtype Drawing = Drawing [(Integer, Integer)] deriving (Eq, Ord, Show)
newtype AlienState = AlienState IntList deriving (Eq, Ord, Show, Read)

-- Make a single interaction with a "protocol". Returns a 'Left' when the
-- protocol is demanding to perform a transmission to the orbital ship. Returns
-- a 'Right' when the protocol has constructed a set of pictures and demands a
-- "click".
interactWith :: alienValue -> AlienState -> IntList -> (AlienState, Either IntList [Drawing])
interactWith interactor (AlienState state) input =
  let response = unsafeCoerce interactor (injectIntList state) (injectIntList input)
    in case extractIntList (A.car response) of
         LInt 0 -> (AlienState . extractIntList $ A.car . A.cdr $ response, Right $ toPictures $ extractIntList $ A.car . A.cdr . A.cdr $ response )
         LInt 1 -> (AlienState . extractIntList $ A.car . A.cdr $ response, Left $ extractIntList $ A.car . A.cdr . A.cdr $ response)
         xs     -> error $ "interactor should return (cons 0/1 ...), got: " ++ show xs
  where
    toPictures LNil = []
    toPictures (LCons x xs) = Drawing (toPicture x) : toPictures xs
    toPictures l = error $ "toPictures " ++ show l
    toPicture LNil = []
    toPicture (LCons x xs) = toPoint x : toPicture xs
    toPicture l = error $ "toPicture " ++ show l
    toPoint (LCons (LInt x) (LInt y)) = (x, y)
    toPoint l = error $ "toPoint " ++ show l
