module Test.Web.IFSC.ClientSpec where

import Prelude

import Affjax (Error, printError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Array (length)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Web.IFSC.Client
  ( BaseUrl(..)
  , WithConfig
  , allFullSeasons
  , getEventFullResults
  , getEventResults
  , getLandingPage
  , getSeasonLeagueResults
  )
import Web.IFSC.Model
  ( Discipline(..)
  , EventId(..)
  , LeagueId(..)
  , ResultUrl(..)
  )

spec :: Spec Unit
spec =
  describe "Client functions"
    $ do
        it "should fetch the landing page successfully"
          $ liftAff testLandingPage
        it "should fetch event results successfully"
          $ liftAff
          $ testEventResults
        it "should fetch event full results successfully"
          $ liftAff
          $ do
              testEventFullResultsMen
              testEventFullResultsWomen
        it "fetches all results for a season"
          $ liftAff testWorkflow
        it "runs the whole workflow" $
          liftAff testWorkflow
        it "filters by discipline" $ do
          liftAff testBoulderOnly
          liftAff testLeadOnly

expectResult :: forall a. Show a => WithConfig (ExceptT Error Aff) a -> (a -> Aff Unit) -> Aff Unit
expectResult clientMethod expect =
  (runExceptT $ runReaderT clientMethod (BaseUrl "http://localhost:8080")) >>=
    ( case _ of
        Right a -> expect a
        Left e -> fail $ printError e
    )

smokeTestClientFunction :: forall a. Show a => WithConfig (ExceptT Error Aff) a -> Aff Unit
smokeTestClientFunction clientMethod =
  expectResult clientMethod (void <<< pure)

testLandingPage :: Aff Unit
testLandingPage = smokeTestClientFunction getLandingPage

testSeasonLeagueResults :: Aff Unit
testSeasonLeagueResults = smokeTestClientFunction $
  getSeasonLeagueResults (LeagueId 1234)

testEventResults :: Aff Unit
testEventResults = smokeTestClientFunction $ getEventResults (EventId 9246)

testEventFullResultsMen :: Aff Unit
testEventFullResultsMen = smokeTestClientFunction $
  getEventFullResults (ResultUrl "/api/v1/events/9246/result/5")

testEventFullResultsWomen :: Aff Unit
testEventFullResultsWomen = smokeTestClientFunction $
  getEventFullResults (ResultUrl "/api/v1/events/9246/result/12")

testWorkflow :: Aff Unit
testWorkflow =
  smokeTestClientFunction $ allFullSeasons Boulder

testBoulderOnly :: Aff Unit
testBoulderOnly =
  expectResult (allFullSeasons Boulder)
    ( \result ->
        length result `shouldEqual` 2
    )

-- there are no lead events in the sample data
testLeadOnly :: Aff Unit
testLeadOnly =
  expectResult (allFullSeasons Lead)
    ( \result ->
        length result `shouldEqual` 0
    )
