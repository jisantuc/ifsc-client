module Web.IFSC.Client where

import Prelude
import Affjax (Error(..), Response)
import Affjax.Node (get)
import Affjax.ResponseFormat (json)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Web.IFSC.Model (LandingPage)

adaptError :: JsonDecodeError -> Error
adaptError jsErr =
  RequestContentError
    ( "Request failed to produce a meaningful response: " <> printJsonDecodeError jsErr
    )

getDecodedBody ::
  forall a.
  DecodeJson a =>
  Either Error (Response Json) ->
  Either Error a
getDecodedBody = case _ of
  Right a ->
    ( lmap adaptError
        <<< decodeJson
        <<< _.body
        $ a
    )
  Left e -> Left e

getLandingPage ::
  Aff
    ( Either Error
        (Response LandingPage)
    )
getLandingPage = getDecodedBody <$> get json "https://components.ifsc-climbing.org/results-api.php?api=index"
