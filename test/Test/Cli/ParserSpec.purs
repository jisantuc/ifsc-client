module Test.Cli.ParserSpec where

import Prelude

import Cli.Parser (EndSeason(..), FetchParams(..), StartSeason(..), progOpts)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Options.Applicative (defaultPrefs, execParserPure, getParseResult)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web.IFSC.Client (BaseUrl(..))
import Web.IFSC.Model (Discipline(..), SeasonName(..))

spec :: Spec Unit
spec =
  describe "Command parser" $ do
    it "with all long options" $
      parseTest
        [ "--from-year", "2005", "--to-year", "2010", "--discipline", "lead" ]
        (FetchParams (StartSeason $ SeasonName 2005) (EndSeason $ SeasonName 2010) Lead defaultBaseUrl)
    it "with all short options" $
      parseTest
        [ "-f", "2005", "-t", "2010", "-d", "lead" ]
        (FetchParams (StartSeason $ SeasonName 2005) (EndSeason $ SeasonName 2010) Lead defaultBaseUrl)
    it "with missing discipline" $
      parseTest [ "-f", "2005", "-t", "2013" ]
        (FetchParams (StartSeason $ SeasonName 2005) (EndSeason $ SeasonName 2013) Boulder defaultBaseUrl)
    it "with missing start year" $
      parseTest [ "-t", "2013", "-d", "speed" ]
        (FetchParams (StartSeason $ SeasonName 2010) (EndSeason $ SeasonName 2013) Speed defaultBaseUrl)
    it "with missing end year" $
      parseTest [ "-f", "2001", "-d", "boulder&lead" ]
        (FetchParams (StartSeason $ SeasonName 2001) (EndSeason $ SeasonName 2022) BoulderAndLead defaultBaseUrl)

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl "http://localhost:8080"

parseTest :: Array String -> FetchParams -> Aff Unit
parseTest args expectation =
  getParseResult (execParserPure defaultPrefs progOpts args)
    `shouldEqual` Just expectation
