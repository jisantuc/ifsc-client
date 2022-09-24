module Web.IFSC.Client where

import Prelude
import Affjax (Error(..))
import Affjax.Node (get)
import Affjax.ResponseFormat (json)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Web.IFSC.Model (EventResults, LandingPage)

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

getJsonUrl :: forall a. DecodeJson a => String -> Aff (Either Error a)
getJsonUrl url = getDecodedBody <$> get json url

getLandingPage :: Aff (Either Error LandingPage)
getLandingPage = getJsonUrl "https://components.ifsc-climbing.org/results-api.php?api=index"

-- getEvent :: Aff (Either Error Event)
-- getEvent = getJsonUrl "asdf"
getEventResults :: Aff (Either Error EventResults)
getEventResults = getJsonUrl "asdf"
