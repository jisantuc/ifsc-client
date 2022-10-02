module Web.IFSC.Model where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), toString)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.Tuple (Tuple(..))

type LandingPage
  = { id :: SeasonId
    , name :: SeasonName
    , url :: String
    , disciplineKinds :: Array (Tuple Int Discipline)
    , leagues :: Array League
    }

newtype SeasonId
  = SeasonId Int

derive newtype instance DecodeJson SeasonId

newtype SeasonName
  = SeasonName String

derive newtype instance DecodeJson SeasonName

data Discipline
  = Speed
  | Lead
  | Boulder
  | Combined

derive instance Eq Discipline

derive instance Generic Discipline _

instance Show Discipline where
  show = genericShow

instance DecodeJson Discipline where
  decodeJson js = decoderForStringMap js $ M.fromFoldable [
    Tuple "speed" Speed,
    Tuple "lead" Lead,
    Tuple "boulder" Boulder,
    Tuple "combined" Combined
  ]

type League
  = { id :: Int
    , name :: LeagueName
    }

newtype LeagueName
  = LeagueName String

derive newtype instance DecodeJson LeagueName

newtype LeagueId
  = LeagueId Int

worldCupsAndWorldChampionships :: LeagueName
worldCupsAndWorldChampionships = LeagueName "World Cups and World Championships"

type Event
  = { localStartDate :: Date
    , localEndDate :: Date
    , event :: String
    , url :: String
    }

type EventResults
  = { category :: CompetitionCategory
    , discipline :: Discipline
    , fullResultsUrl :: String
    }

data CompetitionCategory
  = Men
  | Women

derive instance Eq CompetitionCategory

derive instance Generic CompetitionCategory _

instance Show CompetitionCategory where
  show = genericShow

instance DecodeJson CompetitionCategory where
  decodeJson js = decoderForStringMap js $ M.fromFoldable [
    Tuple "men" Men,
    Tuple "women" Women
  ]

type EventFullResults
  = { ranking :: Array CompetitorResult
    }

type CompetitorResult
  = { firstname :: String
    , lastName :: String
    , rounds :: Array Round
    }

type Round
  = { roundName :: RoundName
    , score :: ScoreString
    , ascents :: Array Ascent
    }

data RoundName
  = Qualification
  | SemiFinal
  | Final

newtype ScoreString
  = ScoreString String

type Ascent
  = { top :: Boolean
    , zone :: Boolean
    , topTries :: Int
    , zoneTries :: Int
    }

decoderForStringMap :: forall a. Json -> M.Map String a -> Either JsonDecodeError a
decoderForStringMap js m =
  let lookupResult = toLower <$> toString js >>= (flip M.lookup) m
  in
    maybe (Left $ UnexpectedValue js) Right lookupResult