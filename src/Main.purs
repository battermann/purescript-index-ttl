module Main where

import Prelude
import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (fromAff, Promise)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (filter, mapMaybe, takeEnd)
import Data.Bifunctor (lmap)
import Data.Date (diff)
import Data.DateTime (Date, canonicalDate, date)
import Data.Either (Either(..), either, note)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Formatter.DateTime (unformatDateTime)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)

newtype Hostname
  = Hostname String

newtype Port
  = Port Int

newtype Ttl
  = Ttl Days

newtype Today
  = Today Date

newtype Index
  = Index String

instance showIndex :: Show Index where
  show (Index index) = index

type DatedIndex
  = { name :: Index, date :: Date }

type ElasticsearchClient
  = { indices :: Aff (Array Index)
    , deleteIndex :: Index -> Aff Unit
    }

findExpired :: Today -> Ttl -> Array DatedIndex -> Array DatedIndex
findExpired (Today today) (Ttl ttl) = filter (\indexDate -> (diff today indexDate.date) > ttl)

determineDate :: Index -> Maybe DatedIndex
determineDate (Index name) = { name: Index name, date: _ } <$> dateOrError
  where
  parsed = name # split (Pattern "_") # takeEnd 3

  dateOrError = case parsed of
    [ year, month, day ] ->
      canonicalDate
        <$> strToEnum year
        <*> strToEnum month
        <*> strToEnum day
    _ -> Nothing

  strToEnum ∷ forall e. BoundedEnum e => String -> Maybe e
  strToEnum = fromString >=> toEnum

deleteExpiredIndices :: ElasticsearchClient -> Today -> Ttl -> Aff Unit
deleteExpiredIndices client today ttl = do
  indices <- client.indices
  let
    datedIndices = indices # mapMaybe determineDate

    expired = findExpired today ttl datedIndices
  void $ client.deleteIndex `traverse` (expired # map _.name)

main :: String -> String -> String -> String -> Effect (Promise Unit)
main datetime ttl hostname port = do
  fromAff $ deleteExpiredIndices
    <$> maybeClient
    <*> dateOrError
    <*> ttlOrError
    # sequence
    >>= either log pure
  where
  dateOrError = unformatDateTime "YYYY-MM-DDTHH:mm:ssZ" datetime # map (date >>> Today)

  ttlOrError = fromString ttl # map (toNumber >>> Days >>> Ttl) # note "Invalid TTL"

  portOrError = fromString port # map Port # note "Invalid port"

  maybeClient = elasticsearchClient (Hostname hostname) <$> portOrError

type ElasticsearchIndex
  = { index :: String }

elasticsearchClient :: Hostname -> Port -> ElasticsearchClient
elasticsearchClient (Hostname hostname) (Port port) =
  { indices:
    indices
      <#> (map _.body)
      <#> (lmap printError)
      <#> (_ >>= indexFromJson)
      >>= either (\err -> log err $> []) pure
      <#> (map (_.index >>> Index))
  , deleteIndex:
    \(Index index) ->
      delete index >>= either (printError >>> log) (const (log $ "Deleted index: " <> index))
  }
  where
  url = "http://" <> hostname <> ":" <> show port

  indexFromJson :: Json -> Either String (Array ElasticsearchIndex)
  indexFromJson = decodeJson

  indices =
    AX.request
      ( AX.defaultRequest
          { url = url <> "/_cat/indices?format=json"
          , method = Left GET
          , responseFormat = ResponseFormat.json
          }
      )

  delete index =
    AX.request
      ( AX.defaultRequest
          { url = url <> "/" <> index
          , method = Left DELETE
          , responseFormat = ResponseFormat.ignore
          }
      )
