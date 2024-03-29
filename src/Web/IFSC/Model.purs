module Web.IFSC.Model where

import Prelude

import Control.Alternative ((<|>))
import Csv as Csv
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), toObject, toString, (.:))
import Data.Array (intercalate, (:))
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.String (toLower)
import Data.Tuple (Tuple(..))

newtype EventId = EventId Int

derive newtype instance Show EventId

newtype ResultUrl = ResultUrl String

derive newtype instance Show ResultUrl

derive newtype instance DecodeJson ResultUrl

type LandingPage =
  { seasons :: Array LandingPageSeason
  }

newtype LandingPageSeason = LandingPageSeason
  { seasonId :: SeasonId
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
      name <- jObject .: "name" >>=
        ( \y ->
            note (TypeMismatch "Season name was not an integer") $ SeasonName <$> fromString y
        )
      url <- jObject .: "url"
      disciplineKinds <- jObject .: "discipline_kinds"
      leagues <- jObject .: "leagues"
      Right $ LandingPageSeason { seasonId, name, url, disciplineKinds, leagues }
    Nothing -> Left $ UnexpectedValue json

newtype SeasonId = SeasonId Int

derive newtype instance DecodeJson SeasonId

derive newtype instance Show SeasonId

newtype SeasonName = SeasonName Int

derive newtype instance DecodeJson SeasonName

derive newtype instance Eq SeasonName

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
  decodeJson js = decoderForStringMap js $ M.fromFoldable
    [ Tuple "speed" Speed
    , Tuple "lead" Lead
    , Tuple "boulder" Boulder
    , Tuple "combined" Combined
    , Tuple "boulder&lead" BoulderAndLead
    ]

type League =
  { id :: LeagueId
  , name :: LeagueName
  }

newtype LeagueName = LeagueName String

derive newtype instance DecodeJson LeagueName

derive newtype instance Eq LeagueName

derive newtype instance Show LeagueName

newtype LeagueId = LeagueId Int

derive newtype instance Show LeagueId

derive newtype instance DecodeJson LeagueId

newtype EventName = EventName String

derive newtype instance DecodeJson EventName

derive newtype instance Show EventName

newtype CompetitorName = CompetitorName String

derive newtype instance DecodeJson CompetitorName

derive newtype instance Show CompetitorName

worldCupsAndWorldChampionships :: LeagueName
worldCupsAndWorldChampionships = LeagueName "World Cups and World Championships"

type SeasonLeagueResults =
  { events :: Array Event
  }

-- note that the url ends with the event id, e.g., /api/v1/events/<id>
-- also this is missing localStartDate and localEndDate because I'm being lazy
-- about converting from json to dates for now; broad strokes statements are
-- maybe enough to get started
type Event =
  { event :: EventName
  , url :: String
  }

type EventResultsPage =
  { d_cats :: Array EventResult
  , name :: EventName
  }

disciplineCategoryResults :: EventResultsPage -> Array EventResult
disciplineCategoryResults = _.d_cats

newtype EventResult = EventResult
  { category :: CompetitionCategory
  , discipline :: Discipline
  , fullResultsUrl :: ResultUrl
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

type NamedEventResult =
  { eventName :: EventName
  , category :: CompetitionCategory
  , discipline :: Discipline
  , fullResultsUrl :: ResultUrl
  }

data CompetitionCategory
  = Men
  | Women

derive instance Eq CompetitionCategory

derive instance Generic CompetitionCategory _

instance Show CompetitionCategory where
  show = genericShow

instance DecodeJson CompetitionCategory where
  decodeJson js = decoderForStringMap js $ M.fromFoldable
    [ Tuple "men" Men
    , Tuple "women" Women
    ]

type EventFullResults =
  { ranking :: Array CompetitorResult
  }

type CategorizedEventFullResults =
  { rank :: Array CompetitorResult
  , eventName :: EventName
  , category :: CompetitionCategory
  }

newtype CompetitorResult = CompetitorResult
  { firstName :: String
  , lastName :: String
  , rank :: Int
  , rounds :: Array Round
  }

derive newtype instance Show CompetitorResult

instance DecodeJson CompetitorResult where
  decodeJson json = case toObject json of
    Just jObject -> do
      firstName <- jObject .: "firstname"
      lastName <- jObject .: "lastname"
      rank <- fromMaybe 1000 <$> jObject .: "rank"
      rounds <- jObject .: "rounds"
      pure $ CompetitorResult { firstName, lastName, rank, rounds }
    Nothing -> Left $ UnexpectedValue json

newtype Round = Round
  { roundName :: RoundName
  , score :: ScoreString
  , ascents :: Array Ascent
  }

derive newtype instance Show Round

instance DecodeJson Round where
  decodeJson json = case toObject json of
    Just jObject -> do
      roundName <- jObject .: "round_name"
      score <- jObject .: "score"
      -- the shape of the API responses changes over time, so decoding has to be
      -- flexible enough to handle several formats
      ascents <- jObject .: "ascents"
        <|>
          ( jObject .: "speed_elimination_stages" >>= (\obj -> obj .: "ascents")
          )
        <|> (jObject .: "speed_elimination_stages")
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
  decodeJson js = decoderForStringMap js $ M.fromFoldable
    [ Tuple "qualification" Qualification
    , Tuple "semi-final" SemiFinal
    , Tuple "final" Final
    ]

newtype ScoreString = ScoreString String

derive newtype instance DecodeJson ScoreString

derive newtype instance Show ScoreString

newtype Ascent = Ascent
  { top :: Boolean
  , zone :: Boolean
  , topTries :: Maybe Int
  , zoneTries :: Maybe Int
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
  let
    lookupResult = toLower <$> toString js >>= (flip M.lookup) m
  in
    maybe (Left $ UnexpectedValue js) Right lookupResult

type ResultAnalysisRow =
  { year :: SeasonName
  , eventName :: EventName
  , competitorName :: CompetitorName
  , rank :: Int
  , round :: RoundName
  , top :: Boolean
  , topTries :: Maybe Int
  , zone :: Boolean
  , zoneTries :: Maybe Int
  , competitionCategory :: CompetitionCategory
  }

header :: String
header = intercalate ","
  [ "year"
  , "eventName"
  , "competitorName"
  , "rank"
  , "round"
  , "top"
  , "topTries"
  , "zone"
  , "zoneTries"
  , "competitionCategory"
  ]

fromEventFullResults
  :: SeasonName
  -> CategorizedEventFullResults
  -> Array ResultAnalysisRow
fromEventFullResults
  seasonName
  eventFullResults = do
  (CompetitorResult crData@{ firstName, lastName, rounds }) <- eventFullResults.rank
  (Round round) <- rounds
  (Ascent ascent) <- round.ascents
  pure $
    { year: seasonName
    , eventName: eventFullResults.eventName
    , competitorName: CompetitorName $ firstName <> " " <> lastName
    , rank: crData.rank
    , round: round.roundName
    , top: ascent.top
    , topTries: ascent.topTries
    , zone: ascent.zone
    , zoneTries: ascent.zoneTries
    , competitionCategory: eventFullResults.category
    }

toCsvLine :: ResultAnalysisRow -> String
toCsvLine
  { year
  , eventName
  , competitorName
  , rank
  , round
  , top
  , topTries
  , zone
  , zoneTries
  , competitionCategory
  } =
  intercalate ","
    [ show year
    , show eventName
    , show competitorName
    , show rank
    , show round
    , show top
    , Csv.optional topTries
    , show zone
    , Csv.optional zoneTries
    , show competitionCategory
    ]

toCsv :: Array ResultAnalysisRow -> String
toCsv results =
  intercalate "\n" $ header : (toCsvLine <$> results)
