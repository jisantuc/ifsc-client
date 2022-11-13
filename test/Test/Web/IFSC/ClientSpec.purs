module Test.Web.IFSC.ClientSpec where

import Prelude

import Affjax (Error, printError)
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either, isRight)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Web.IFSC.Client (BaseUrl(..), WithConfig, getEventFullResults, getEventResults, getLandingPage, getSeasonLeagueResults)
import Web.IFSC.Model (EventId(..), LeagueId(..), ResultUrl(..))

spec :: Spec Unit
spec =
  describe "Client functions"
    $ do
        it "should fetch the landing page successfully"
          $ liftAff testLandingPage
        it "should fetch event results successfully"
          $ liftAff $ testEventResults
        it "should fetch event full results successfully"
          $ liftAff $ do
            testEventFullResultsMen
            testEventFullResultsWomen
        it "fetches all results for a season"
          $ liftAff testWorkflow


-- what do I want to tesT?
-- making a request to the url on the wiremock server
-- results in decoding successfully?
-- results in the specific data?
-- can can encode <-> decode?
adaptError :: forall a. Either Error a -> Either String a
adaptError = lmap printError

smokeTestClientFunction :: forall a. Show a => WithConfig Aff (Either Error a) -> Aff Unit
smokeTestClientFunction clientMethod = 
  let
    test = do
       result <- clientMethod
       let
         adapted = adaptError result
       adapted `shouldSatisfy` isRight
  in runReaderT test (BaseUrl "http://localhost:8080") 

testLandingPage :: Aff Unit
testLandingPage = smokeTestClientFunction getLandingPage

testSeasonLeagueResults :: Aff Unit
testSeasonLeagueResults = smokeTestClientFunction $ getSeasonLeagueResults (LeagueId 1234)

testEventResults :: Aff Unit
testEventResults = smokeTestClientFunction $ getEventResults (EventId 9246)

testEventFullResultsMen :: Aff Unit
testEventFullResultsMen = smokeTestClientFunction $ getEventFullResults (ResultUrl "/api/v1/events/9246/result/5")

testEventFullResultsWomen :: Aff Unit
testEventFullResultsWomen = smokeTestClientFunction $ getEventFullResults (ResultUrl "/api/v1/events/9246/result/12")

testWorkflow :: Aff Unit
testWorkflow =
  pure unit

