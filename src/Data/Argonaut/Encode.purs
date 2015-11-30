module Data.Argonaut.Encode
  ( EncodeJson
  , encodeJson
  , gEncodeJson
  , gEncodeJson'
  ) where

import Prelude

import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject, JAssoc(), foldJsonObject, jsonSingletonObject, jsonEmptyObject)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic (Generic, GenericSpine(..), toSpine)
import Data.Int (toNumber)
import Data.List (List(), fromList)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (fromChar)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Data.Rational (Rational (..))
import Data.Ratio (numerator, denominator)
import Data.Date (Date(), JSDate(), toJSDate)

class EncodeJson a where
  encodeJson :: a -> Json

-- | Encode any `Generic` data structure into `Json`.
gEncodeJson :: forall a. (Generic a) => a -> Json
gEncodeJson = gEncodeJson' <<< toSpine

-- | Encode `GenericSpine` into `Json`.
gEncodeJson' :: GenericSpine -> Json
gEncodeJson' spine = case spine of
  SInt x            -> fromNumber $ toNumber x
  SString x         -> fromString x
  SChar x           -> fromString $ fromChar x
  SNumber x         -> fromNumber x
  SBoolean x        -> fromBoolean x
  SArray thunks     -> fromArray (gEncodeJson' <<< (unit #) <$> thunks)
  SProd constr args -> fromObject
                         $ SM.insert    "tag"    (encodeJson constr)
                         $ SM.singleton "values" (encodeJson (gEncodeJson' <<< (unit #) <$> args))
  SRecord fields    -> fromObject $ foldr addField SM.empty fields
    where addField field = SM.insert field.recLabel
                                     (gEncodeJson' $ field.recValue unit)

instance encodeJsonMaybe :: (EncodeJson a) => EncodeJson (Maybe a) where
  encodeJson Nothing  = jsonNull
  encodeJson (Just a) = encodeJson a

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson (Left a) = encodeJson a
  encodeJson (Right b) = encodeJson b

instance encodeJsonUnit :: EncodeJson Unit where
  encodeJson = const jsonNull

instance encodeJsonJBoolean :: EncodeJson Boolean where
  encodeJson = fromBoolean

instance encodeJsonJNumber :: EncodeJson Number where
  encodeJson = fromNumber

instance encodeJsonInt :: EncodeJson Int where
  encodeJson = fromNumber <<< toNumber

instance encodeJsonJString :: EncodeJson String where
  encodeJson = fromString

instance encodeJsonJson :: EncodeJson Json where
  encodeJson = id

instance encodeJsonChar :: EncodeJson Char where
  encodeJson = encodeJson <<< fromChar

instance encodeJsonArray :: (EncodeJson a) => EncodeJson (Array a) where
  encodeJson json = fromArray (encodeJson <$> json)

instance encodeJsonList :: (EncodeJson a) => EncodeJson (List a) where
  encodeJson json =
    let arr :: Array a
        arr = fromList json
    in fromArray (encodeJson <$> arr)

instance encodeStrMap :: (EncodeJson a) => EncodeJson (SM.StrMap a) where
  encodeJson m = fromObject (encodeJson <$> m)

instance encodeMap :: (Ord a, EncodeJson a, EncodeJson b) => EncodeJson (M.Map a b) where
  encodeJson = encodeJson <<< M.toList


infix 7 :=
(:=) :: forall a. (EncodeJson a) => String -> a -> JAssoc
(:=) k v = Tuple k $ encodeJson v

infixr 6 ~>
(~>) :: forall a. (EncodeJson a) => JAssoc -> a -> Json
(~>) (Tuple k v) a = foldJsonObject (jsonSingletonObject k v) (SM.insert k v >>> fromObject) (encodeJson a)

instance encodeDate :: EncodeJson Date where
  encodeJson = fromString <<< dateToISO <<< toJSDate

instance encodeRational :: EncodeJson Rational where
  encodeJson (Rational rat)
    =  "denominator" := (denominator rat)
    ~> "numerator" := (numerator rat)
    ~> jsonEmptyObject

foreign import dateToISO :: JSDate -> String
