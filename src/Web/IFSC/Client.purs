module Web.IFSC.Client where

import Prelude

import Affjax (Error(..))
import Affjax.Node (get)
import Affjax.ResponseFormat (json)
import Control.Monad.Except (ExceptT(..), except)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (ask)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (filter, last)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (fold)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Web.IFSC.Model
  ( Discipline
  , Event
  , EventFullResults
  , EventId(..)
  , EventResult(..)
  , LandingPage
  , LandingPageSeason(..)
  , LeagueId(..)
  , ResultUrl(..)
  , SeasonLeagueResults
  , SeasonName(..)
  , disciplineCategoryResults
  )

newtype BaseUrl = BaseUrl String

type WithConfig :: forall k. (k -> Type) -> k -> Type
type WithConfig m a = ReaderT BaseUrl m a

getEventId :: Event -> Either Error EventId
getEventId { url } =
  let
    segments = split (Pattern "/") url
    lastSegment = note (RequestContentError "Url segment was empty") $ last segments
    eventId = lastSegment >>=
      ( \s ->
          note (RequestContentError $ "Could not read last url segment to an int in url: " <> url) $
            EventId <$> fromString s
      )
  in
    eventId

adaptError :: JsonDecodeError -> Error
adaptError jsErr =
  RequestContentError
    ( "Request failed to produce a meaningful response: " <> printJsonDecodeError jsErr
    )

getDecodedBody
  :: forall a
   . forall r
   . DecodeJson a
  => Either Error ({ body :: Json | r })
  -> Either Error a
getDecodedBody = case _ of
  Right a ->
    ( lmap adaptError
        <<< decodeJson
        $ a.body
    )
  Left e -> Left e

getJsonUrl :: forall a. DecodeJson a => String -> WithConfig (ExceptT Error Aff) a
getJsonUrl urlPart = do
  BaseUrl base <- ask
  lift <<< ExceptT $ getDecodedBody <$> get json (base <> urlPart)

getLandingPage :: WithConfig (ExceptT Error Aff) LandingPage
getLandingPage = getJsonUrl "/results-api.php?api=index"

getSeasonLeagueResults :: LeagueId -> WithConfig (ExceptT Error Aff) SeasonLeagueResults
getSeasonLeagueResults (LeagueId league) =
  getJsonUrl $ "/results-api.php?api=season_league_results&league=" <> show league

getEventResults :: EventId -> WithConfig (ExceptT Error Aff) (Array EventResult)
getEventResults eventId =
  disciplineCategoryResults <$> (getJsonUrl $ "/results-api.php?api=event_results&event_id=" <> show eventId)

getEventFullResults :: ResultUrl -> WithConfig (ExceptT Error Aff) EventFullResults
getEventFullResults (ResultUrl queryParam) =
  getJsonUrl $ "/results-api.php?api=event_full_results&result_url=" <> queryParam

fullSeasons :: Discipline -> Maybe Int -> Maybe Int -> WithConfig (ExceptT Error Aff) (Array EventFullResults)
fullSeasons searchDiscipline fromYear toYear =
  let
    inRange =
      ( \(LandingPageSeason { name }) ->
          let
            (SeasonName y) = name
          in
            fromMaybe true ((y >= _) <$> fromYear) &&
              fromMaybe true ((y <= _) <$> toYear)
      )
  in
    do
      { seasons } <- getLandingPage
      let seasonsInRange = filter inRange seasons
      seasonLeagueEvents <- traverse
        ( \(LandingPageSeason { leagues }) ->
            let
              leagueIds = _.id <$> leagues
            in
              -- for each league, get league results
              (_.events <$> _) <$> traverse getSeasonLeagueResults leagueIds
        )
        seasonsInRange
      let allEvents = join <<< join $ seasonLeagueEvents
      eventIds <- lift <<< except $ traverse getEventId allEvents
      eventPartialResultsArrArr <- traverse getEventResults eventIds
      let
        eventPartialResults =
          filter
            ( \(EventResult { discipline }) ->
                discipline == searchDiscipline
            ) $ fold eventPartialResultsArrArr
      allFullResults <- traverse getEventFullResults ((\(EventResult { fullResultsUrl }) -> fullResultsUrl) <$> eventPartialResults)
      pure allFullResults

allFullSeasons :: Discipline -> ReaderT BaseUrl (ExceptT Error Aff) (Array EventFullResults)
allFullSeasons discipline = fullSeasons discipline Nothing Nothing
