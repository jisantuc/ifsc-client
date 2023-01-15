module Test.Cli.ParserSpec where

import Prelude

import Cli.Parser
  ( EndSeason(..)
  , FetchParams(..)
  , ProgramMode(..)
  , SeasonToCsvParams(..)
  , StartSeason(..)
  , fetchParser
  , progOpts
  , seasonToCsvParser
  )
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Options.Applicative (ParserInfo, defaultPrefs, execParserPure, getParseResult, info, progDesc)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web.IFSC.Client (BaseUrl(..))
import Web.IFSC.Model (Discipline(..), SeasonName(..))

spec :: Spec Unit
spec =
  describe "Command parser" $ do
    describe "FetchParams" $ do
      it "with all long options" $
        fetchParamsParserTest
          [ "--from-year", "2005", "--to-year", "2010", "--discipline", "lead" ]
          (FetchParams (StartSeason $ SeasonName 2005) (EndSeason $ SeasonName 2010) Lead defaultBaseUrl)
      it "with all short options" $
        fetchParamsParserTest
          [ "-f", "2005", "-t", "2010", "-d", "lead" ]
          (FetchParams (StartSeason $ SeasonName 2005) (EndSeason $ SeasonName 2010) Lead defaultBaseUrl)
      it "with missing discipline" $
        fetchParamsParserTest [ "-f", "2005", "-t", "2013" ]
          (FetchParams (StartSeason $ SeasonName 2005) (EndSeason $ SeasonName 2013) Boulder defaultBaseUrl)
      it "with missing start year" $
        fetchParamsParserTest [ "-t", "2013", "-d", "speed" ]
          (FetchParams (StartSeason $ SeasonName 2010) (EndSeason $ SeasonName 2013) Speed defaultBaseUrl)
      it "with missing end year" $
        fetchParamsParserTest [ "-f", "2001", "-d", "boulder&lead" ]
          (FetchParams (StartSeason $ SeasonName 2001) (EndSeason $ SeasonName 2022) BoulderAndLead defaultBaseUrl)
    describe "SeasonToCsvParams" $ do
      it "with all long options" $
        seasonToCsvParserTest [ "--for-year", "2015", "--discipline", "boulder" ]
          (SeasonToCsvParams (SeasonName 2015) Boulder defaultBaseUrl)
      it "with all short options" $
        seasonToCsvParserTest [ "-y", "2018", "-d", "boulder" ]
          (SeasonToCsvParams (SeasonName 2018) Boulder defaultBaseUrl)
      it "with missing discipline" $
        seasonToCsvParserTest [ "-y", "2021" ]
          (SeasonToCsvParams (SeasonName 2021) Boulder defaultBaseUrl)
    describe "Command parser" $ do
      it "season-to-csv" $
        parseTest
          [ "season-to-csv", "-y", "2010" ]
          progOpts
          (SeasonToCsv (SeasonToCsvParams (SeasonName 2010) Boulder defaultBaseUrl))
      it "fetch-seasons-validate" $ do
        parseTest
          [ "fetch-seasons-validate", "-f", "2009", "-t", "2015" ]
          progOpts
          ( FetchSeasons
              ( FetchParams
                  (StartSeason $ SeasonName 2009)
                  (EndSeason $ SeasonName 2015)
                  Boulder
                  defaultBaseUrl
              )
          )

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl "http://localhost:8080"

parseTest :: forall a. Show a => Eq a => Array String -> ParserInfo a -> a -> Aff Unit
parseTest args parser expectation =
  getParseResult (execParserPure defaultPrefs parser args)
    `shouldEqual` Just expectation

fetchParamsParserTest :: Array String -> FetchParams -> Aff Unit
fetchParamsParserTest args expectation =
  parseTest args (info fetchParser (progDesc "test")) expectation

seasonToCsvParserTest :: Array String -> SeasonToCsvParams -> Aff Unit
seasonToCsvParserTest args expectation =
  parseTest args (info seasonToCsvParser (progDesc "test")) expectation
