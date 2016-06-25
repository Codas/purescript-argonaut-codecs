module Data.Argonaut.Decode.Class
  ( class DecodeJson
  , decodeJson
  , gDecodeJson
  , gDecodeJson'
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Data.Bifunctor (lmap)
import Data.Argonaut.Core (Json, JArray, JObject, isNull, foldJsonNull, foldJsonBoolean, foldJsonNumber, foldJsonString, toArray, toNumber, toObject, toString, toBoolean)
import Data.Array (zipWithA, (!!))
import Data.Either (either, Either(..))
import Data.Foldable (find)
import Data.Generic (class Generic, GenericSpine(..), GenericSignature(..), fromSpine, toSignature)
import Data.Int (fromNumber, fromString)
import Data.List (List(..), (:), fromFoldable)
import Data.Map as M
import Data.Maybe (maybe, Maybe(..))
import Data.String (charAt, toChar, split)
import Data.StrMap as SM
import Data.Traversable (traverse, for)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Data.Rational (Rational (), (%))
import Data.Date (Date (), canonicalDate)
import Data.Enum (toEnum)

class DecodeJson a where
  decodeJson :: Json -> Either String a

-- | Decode `Json` representation of a value which has a `Generic` type.
gDecodeJson :: forall a. Generic a => Json -> Either String a
gDecodeJson
  = maybe (Left "fromSpine failed") Right
  <<< fromSpine
  <=< gDecodeJson' (toSignature (Proxy :: Proxy a))

-- | Decode `Json` representation of a `GenericSpine`.
gDecodeJson' :: GenericSignature -> Json -> Either String GenericSpine
gDecodeJson' signature json = case signature of
  SigNumber -> SNumber <$> mFail "Expected a number" (toNumber json)
  SigInt -> SInt <$> mFail "Expected an integer number" (fromNumber =<< toNumber json)
  SigString -> SString <$> mFail "Expected a string" (toString json)
  SigChar -> SChar <$> mFail "Expected a char" (toChar =<< toString json)
  SigBoolean -> SBoolean <$> mFail "Expected a boolean" (toBoolean json)
  SigUnit -> pure SUnit
  SigArray thunk -> do
    jArr <- mFail "Expected an array" $ toArray json
    SArray <$> traverse (map const <<< gDecodeJson' (thunk unit)) jArr
  SigRecord props -> do
    jObj <- mFail "Expected an object" $ toObject json
    SRecord <$> for props \({recLabel: lbl, recValue: val}) -> do
      pf <- mFail ("'" <> lbl <> "' property missing") (SM.lookup lbl jObj)
      sp <- gDecodeJson' (val unit) pf
      pure { recLabel: lbl, recValue: const sp }
  SigProd typeConstr alts -> do
    let decodingErr msg = "When decoding a " <> typeConstr <> ": " <> msg
    jObj <- mFail (decodingErr "expected an object") (toObject json)
    tagJson  <- mFail (decodingErr "'tag' property is missing") (SM.lookup "tag" jObj)
    tag <- mFail (decodingErr "'tag' property is not a string") (toString tagJson)
    case find ((tag == _) <<< _.sigConstructor) alts of
      Nothing -> Left (decodingErr ("'" <> tag <> "' isn't a valid constructor"))
      Just { sigValues: sigValues } -> do
        vals <- mFail (decodingErr "'values' array is missing") (toArray =<< SM.lookup "values" jObj)
        sps  <- zipWithA (\k -> gDecodeJson' (k unit)) sigValues vals
        pure (SProd tag (const <$> sps))
  where
  mFail :: forall a. String -> Maybe a -> Either String a
  mFail msg = maybe (Left msg) Right

instance decodeJsonMaybe :: DecodeJson a => DecodeJson (Maybe a) where
  decodeJson j
    | isNull j = pure Nothing
    | otherwise = Just <$> decodeJson j

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson j = decodeJson j >>= f
    where
    f (a : b : Nil) = Tuple <$> decodeJson a <*> decodeJson b
    f _ = Left "Couldn't decode Tuple"

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson json =
    lmap ("Couldn't decode Either: " <> _) $
      decodeJOject json >>= \obj -> do
        tag <- maybe (Left "Expected field 'tag'") Right $ SM.lookup "tag" obj
        val <- maybe (Left "Expected field 'value'") Right $ SM.lookup "value" obj
        case toString tag of
          Just "Right" -> Right <$> decodeJson val
          Just "Left" -> Left <$> decodeJson val
          _ -> Left "'tag' field was not \"Left\" or \"Right\""

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = foldJsonNull (Left "Value is not a null") (const $ Right unit)

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = foldJsonBoolean (Left "Value is not a Boolean") Right

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = foldJsonNumber (Left "Value is not a Number") Right

instance decodeJsonInt :: DecodeJson Int where
  decodeJson
    = maybe (Left "Value is not an integer") Right
    <<< fromNumber
    <=< decodeJson

instance decodeJsonString :: DecodeJson String where
  decodeJson = foldJsonString (Left "Value is not a String") Right

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonChar :: DecodeJson Char where
  decodeJson j =
    maybe (Left $ "Expected character but found: " <> show j) Right
      =<< charAt 0 <$> decodeJson j

instance decodeStrMap :: DecodeJson a => DecodeJson (SM.StrMap a) where
  decodeJson
    = lmap ("Couldn't decode StrMap: " <> _)
    <<< (traverse decodeJson <=< decodeJOject)

instance decodeArray :: DecodeJson a => DecodeJson (Array a) where
  decodeJson
    = lmap ("Couldn't decode Array: " <> _)
    <<< (traverse decodeJson <=< decodeJArray)

instance decodeList :: DecodeJson a => DecodeJson (List a) where
  decodeJson
    = lmap ("Couldn't decode List: " <> _)
    <<< (traverse decodeJson <=< map (map fromFoldable) decodeJArray)

instance decodeMap :: (Ord a, DecodeJson a, DecodeJson b) => DecodeJson (M.Map a b) where
  decodeJson = map M.fromList <<< decodeJson


instance decodeJsonRational :: DecodeJson Rational where
  decodeJson json = do
    obj <- decodeJson json
    denom <- obj .? "denominator"
    num <- obj .? "numerator"
    pure (num % denom)

instance decodeJsonDate :: DecodeJson Date where
  decodeJson = foldJsonString (Left "Not a String") dateFromString
    where dateFromString s =
            let pieces = split "-" s
                mys :: Maybe String
                mys = pieces !! 0
                mms = pieces !! 1
                mds = pieces !! 2
                mDate = do ys <- mys
                           ms <- mms
                           ds <- mds
                           yi <- fromString ys
                           mi <- fromString ms
                           di <- fromString ds
                           y <- toEnum yi
                           m <- toEnum mi
                           d <- toEnum di
                           pure (canonicalDate y m d)
                eDate = case mDate of
                          Just d -> Right d
                          Nothing -> Left "String is not a valid Date"
            in eDate

decodeJArray :: Json -> Either String JArray
decodeJArray = maybe (Left "Value is not an Array") Right <<< toArray

decodeJOject :: Json -> Either String JObject
decodeJOject = maybe (Left "Value is not an Object") Right <<< toObject

decodeMaybe :: forall a. DecodeJson a => Json -> Maybe a
decodeMaybe = either (const Nothing) pure <<< decodeJson



getField :: forall a. DecodeJson a => JObject -> String -> Either String a
getField o s =
  maybe
    (Left $ "Expected field " <> show s)
    decodeJson
    (SM.lookup s o)

infix 7 getField as .?
