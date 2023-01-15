module Web.IFSC.Client where

import Prelude

import Affjax (Error(..))
import Affjax.Node (get)
import Affjax.ResponseFormat (json)
import Affjax.ResponseHeader (ResponseHeader)
import Control.Monad.Except (ExceptT(..), except)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (ask)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, encodeJson, printJsonDecodeError)
import Data.Array (filter, last)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (fold, intercalate)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Web.IFSC.Model
  ( Discipline
  , Event
  , EventFullResults
  , EventId(..)
  , EventResult(..)
  , LandingPage
  , LandingPageSeason(..)
  , LeagueId(..)
  , LeagueName(..)
  , ResultUrl(..)
  , SeasonLeagueResults
  , SeasonName(..)
  , disciplineCategoryResults
  )

data FetchError = FetchError Error String Json

newtype BaseUrl = BaseUrl String

derive newtype instance Eq BaseUrl

derive newtype instance Show BaseUrl

type WithConfig :: forall k. (k -> Type) -> k -> Type
type WithConfig m a = ReaderT BaseUrl m a

getEventId :: Event -> Either FetchError EventId
getEventId { url } =
  let
    segments = split (Pattern "/") url
    lastSegment =
      note
        ( FetchError (RequestContentError "Url segment was empty") url (encodeJson {})
        ) $
        last segments
    eventId = lastSegment >>=
      ( \s ->
          note
            ( FetchError (RequestContentError $ "Could not read last url segment to an int in url: " <> url) url (encodeJson {})
            ) $
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
  => Either Error ({ body :: Json, headers :: Array ResponseHeader | r })
  -> Either Error a
getDecodedBody = case _ of
  Right a ->
    ( lmap adaptError
        <<< decodeJson
        $ a.body
    )
  Left e -> Left e

getJsonUrl :: forall a. DecodeJson a => String -> WithConfig (ExceptT FetchError Aff) a
getJsonUrl urlPart =
  do
    BaseUrl base <- ask
    let fullUrl = base <> urlPart
    lift <<< ExceptT $
      ( case _ of
          response@(Right { body }) ->
            lmap (\e -> FetchError e fullUrl body) $ getDecodedBody response
          Left e -> Left $ FetchError e fullUrl (encodeJson {})
      ) <$> get json fullUrl

getLandingPage :: WithConfig (ExceptT FetchError Aff) LandingPage
getLandingPage = getJsonUrl "/results-api.php?api=index"

getSeasonLeagueResults :: LeagueId -> WithConfig (ExceptT FetchError Aff) SeasonLeagueResults
getSeasonLeagueResults (LeagueId league) =
  getJsonUrl $ "/results-api.php?api=season_leagues_results&league=" <> show league

getEventResults :: EventId -> WithConfig (ExceptT FetchError Aff) (Array EventResult)
getEventResults eventId = do
  resultsPage <- getJsonUrl $ "/results-api.php?api=event_results&event_id=" <> show eventId
  log $ "Results available for " <> resultsPage.name
  pure $ disciplineCategoryResults resultsPage

getEventFullResults :: ResultUrl -> WithConfig (ExceptT FetchError Aff) EventFullResults
getEventFullResults (ResultUrl queryParam) = do
  BaseUrl baseUrl <- ask
  log $ "Fetching event results at: " <> baseUrl <> queryParam
  getJsonUrl $ "/results-api.php?api=event_full_results&result_url=" <> queryParam

fullSeasons :: Discipline -> Maybe Int -> Maybe Int -> WithConfig (ExceptT FetchError Aff) (Array EventFullResults)
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
      log $ "Fetching events for the following seasons: " <> show ((\(LandingPageSeason { name }) -> name) <$> seasonsInRange)
      seasonLeagueEvents <- traverse
        ( \(LandingPageSeason { leagues }) ->
            let
              leagueIds = _.id <$>
                ( filter
                    ( \league ->
                        league.name == LeagueName "World Cups and World Championships"
                    )
                    leagues
                )
            in
              -- for each league, get league results
              (_.events <$> _) <$> traverse getSeasonLeagueResults leagueIds
        )
        seasonsInRange
      let allEvents = join <<< join $ seasonLeagueEvents
      log $ "All events:\n" <>
        let
          allEventNames = _.event <$> allEvents
        in
          intercalate "\n" allEventNames
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

allFullSeasons :: Discipline -> ReaderT BaseUrl (ExceptT FetchError Aff) (Array EventFullResults)
allFullSeasons discipline = fullSeasons discipline Nothing Nothing

competitorResultsForSeason :: SeasonId -> Discipline -> ReaderT BaseUrl (ExceptT Error Aff) (Array ResultAnalysisRow)
competitorResultsForSeason seasonId discipline =
        pure []
