module Web.IFSC.Model where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), toObject, toString, (.:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.Tuple (Tuple(..))

newtype EventId = EventId Int

derive newtype instance Show EventId

newtype ResultUrl = ResultUrl String

type LandingPage = {
  seasons :: Array LandingPageSeason
}

newtype LandingPageSeason
  = LandingPageSeason { seasonId :: SeasonId
    , name :: SeasonName
    , url :: String
    , disciplineKinds :: Array (Tuple Int Discipline)
    , leagues :: Array League
    }

derive newtype instance Show LandingPageSeason

instance DecodeJson LandingPageSeason where
  decodeJson json = case toObject json of
    Just jObject -> do 
       seasonId <- jObject .: "id"
       name <- jObject .: "name"
       url <- jObject .: "url"
       disciplineKinds <- jObject .: "discipline_kinds"
       leagues <- jObject .: "leagues"
       Right $ LandingPageSeason { seasonId, name, url, disciplineKinds, leagues }
    Nothing -> Left $ UnexpectedValue json

newtype SeasonId
  = SeasonId Int

derive newtype instance DecodeJson SeasonId

derive newtype instance Show SeasonId

newtype SeasonName
  = SeasonName String

derive newtype instance DecodeJson SeasonName

derive newtype instance Show SeasonName

data Discipline
  = Speed
  | Lead
  | Boulder
  | Combined
  | BoulderAndLead

derive instance Eq Discipline

derive instance Generic Discipline _

instance Show Discipline where
  show = genericShow

instance DecodeJson Discipline where
  decodeJson js = decoderForStringMap js $ M.fromFoldable [
    Tuple "speed" Speed,
    Tuple "lead" Lead,
    Tuple "boulder" Boulder,
    Tuple "combined" Combined,
    Tuple "boulder&lead" BoulderAndLead
  ]

type League
  = { id :: Int
    , name :: LeagueName
    }

newtype LeagueName
  = LeagueName String

derive newtype instance DecodeJson LeagueName

derive newtype instance Show LeagueName

newtype LeagueId
  = LeagueId Int

worldCupsAndWorldChampionships :: LeagueName
worldCupsAndWorldChampionships = LeagueName "World Cups and World Championships"

type SeasonLeagueResults = {
  events :: Array Event
}

type Event
  = { event :: String
    , url :: String
    }

type EventResultsPage = {
    d_cats :: Array EventResult
}

disciplineCategoryResults :: EventResultsPage -> Array EventResult
disciplineCategoryResults = _.d_cats

newtype EventResult = EventResult
    { category :: CompetitionCategory
    , discipline :: Discipline
    , fullResultsUrl :: String
    }

derive newtype instance Show EventResult

instance DecodeJson EventResult where
  decodeJson json = case toObject json of
    Just jObject ->
      do 
       category <- jObject .: "category_name"
       discipline <- jObject .: "discipline_kind"
       fullResultsUrl <- jObject .: "full_results_url"
       pure $ EventResult { category, discipline, fullResultsUrl }

    Nothing -> Left $ UnexpectedValue json

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

newtype CompetitorResult
  = CompetitorResult { firstName :: String
    , lastName :: String
    , rounds :: Array Round
    }

derive newtype instance Show CompetitorResult

instance DecodeJson CompetitorResult where
  decodeJson json = case toObject json of
    Just jObject -> do 
       firstName <- jObject .: "firstname"
       lastName <- jObject .: "lastname"
       rounds <- jObject .: "rounds"
       pure $ CompetitorResult { firstName, lastName, rounds }
    Nothing -> Left $ UnexpectedValue json

newtype Round
  = Round { roundName :: RoundName
    , score :: ScoreString
    , ascents :: Array Ascent
    }

derive newtype instance Show Round

instance DecodeJson Round where
  decodeJson json = case toObject json of
    Just jObject -> do
       roundName <- jObject .: "round_name"
       score <- jObject .: "score"
       ascents <- jObject .: "ascents"
       pure $ Round { roundName, score, ascents }
    Nothing -> Left $ UnexpectedValue json

data RoundName
  = Qualification
  | SemiFinal
  | Final

derive instance Eq RoundName

derive instance Generic RoundName _

instance Show RoundName where
  show = genericShow

instance DecodeJson RoundName where
  decodeJson js = decoderForStringMap js $ M.fromFoldable [
    Tuple "qualification" Qualification,
    Tuple "semi-final" SemiFinal,
    Tuple "final" Final
  ]

newtype ScoreString
  = ScoreString String

derive newtype instance DecodeJson ScoreString

derive newtype instance Show ScoreString

newtype Ascent
  = Ascent { top :: Boolean
    , zone :: Boolean
    , topTries :: Int
    , zoneTries :: Int
    }

derive newtype instance Show Ascent

instance DecodeJson Ascent where
  decodeJson json = case toObject json of
    Just jObject -> do 
       top <- jObject .: "top"
       zone <- jObject .: "zone"
       topTries <- jObject .: "top_tries"
       zoneTries <- jObject .: "zone_tries"
       pure $ Ascent { top, zone, topTries, zoneTries }
    Nothing -> Left $ UnexpectedValue json

decoderForStringMap :: forall a. Json -> M.Map String a -> Either JsonDecodeError a
decoderForStringMap js m =
  let lookupResult = toLower <$> toString js >>= (flip M.lookup) m
  in
    maybe (Left $ UnexpectedValue js) Right lookupResult
