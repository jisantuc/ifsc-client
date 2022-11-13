module Web.IFSC.Client where

import Prelude

import Affjax (Error(..))
import Affjax.Node (get)
import Affjax.ResponseFormat (json)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (ask)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Web.IFSC.Model (EventFullResults, EventId, EventResult, LandingPage, LeagueId(..), ResultUrl(..), SeasonLeagueResults, disciplineCategoryResults)

newtype BaseUrl = BaseUrl String

type WithConfig :: forall k. (k -> Type) -> k -> Type
type WithConfig m a = ReaderT BaseUrl m a

adaptError :: JsonDecodeError -> Error
adaptError jsErr =
  RequestContentError
    ( "Request failed to produce a meaningful response: " <> printJsonDecodeError jsErr
    )

getDecodedBody ::
  forall a.
  forall r.
  DecodeJson a =>
  Either Error ({ body :: Json | r }) ->
  Either Error a
getDecodedBody = case _ of
  Right a ->
    ( lmap adaptError
        <<< decodeJson
        $ a.body
    )
  Left e -> Left e

getJsonUrl :: forall a. DecodeJson a => String -> WithConfig Aff (Either Error a)
getJsonUrl urlPart = do
  BaseUrl base <- ask
  lift $ getDecodedBody <$> get json (base <> urlPart)

getLandingPage :: WithConfig Aff (Either Error LandingPage)
getLandingPage = getJsonUrl "/results-api.php?api=index"

getSeasonLeagueResults :: LeagueId -> WithConfig Aff (Either Error SeasonLeagueResults)
getSeasonLeagueResults (LeagueId league) =
  getJsonUrl $ "/results-api.php?api=season_league_results&league_id=" <> show league

getEventResults :: EventId -> WithConfig Aff (Either Error (Array EventResult))
getEventResults eventId =
  ((disciplineCategoryResults <$> _) <$> _)
    (getJsonUrl $ "/results-api.php?api=event_results&event_id=" <> show eventId)

getEventFullResults :: ResultUrl -> WithConfig Aff (Either Error (EventFullResults))
getEventFullResults (ResultUrl queryParam) =
    getJsonUrl $ "/results-api.php?api=event_full_results&result_url=" <> queryParam
