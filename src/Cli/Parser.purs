module Cli.Parser where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Options.Applicative
  ( Parser
  , ParserInfo
  , ReadM
  , command
  , eitherReader
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , int
  , long
  , option
  , progDesc
  , short
  , strOption
  , value
  , (<**>)
  )
import Web.IFSC.Client (BaseUrl(..))
import Web.IFSC.Model (Discipline(..), SeasonName(..))

newtype StartSeason = StartSeason SeasonName

derive newtype instance Eq StartSeason

derive newtype instance Show StartSeason

newtype EndSeason = EndSeason SeasonName

derive newtype instance Eq EndSeason

derive newtype instance Show EndSeason

data FetchParams = FetchParams StartSeason EndSeason Discipline BaseUrl

derive instance Generic FetchParams _

derive instance Eq FetchParams

instance Show FetchParams where
  show = genericShow

data SeasonToCsvParams = SeasonToCsvParams SeasonName Discipline BaseUrl

derive instance Generic SeasonToCsvParams _

derive instance Eq SeasonToCsvParams

instance Show SeasonToCsvParams where
  show = genericShow

data ProgramMode
  = FetchSeasons FetchParams
  | SeasonToCsv SeasonToCsvParams

derive instance Eq ProgramMode

derive instance Generic ProgramMode _

instance Show ProgramMode where
  show = genericShow

startSeason :: Parser StartSeason
startSeason = StartSeason <<< SeasonName <$> option int
  ( long "from-year"
      <> short 'f'
      <> help "First year to consider results from"
      <> value 2010
  )

endSeason :: Parser EndSeason
endSeason = EndSeason <<< SeasonName <$> option int
  ( long "to-year"
      <> short 't'
      <> help "Last year to consider results from"
      <> value 2022
  )

seasonName :: Parser SeasonName
seasonName = SeasonName <$> option int
  ( long "for-year"
      <> short 'y'
      <> help "Year to dump results to csv for"
  )

disciplineReader :: ReadM Discipline
disciplineReader = eitherReader $ case _ of
  "boulder" -> Right Boulder
  "lead" -> Right Lead
  "boulder&lead" -> Right BoulderAndLead
  "speed" -> Right Speed
  s -> Left $ "Invalid discipline: " <> s

discipline :: Parser Discipline
discipline = option disciplineReader
  ( long "discipline"
      <> short 'd'
      <> help "Which discipline to assemble results for. Options are boulder, lead, boulder&lead, and speed"
      <> value Boulder
  )

baseUrl :: Parser BaseUrl
baseUrl = BaseUrl <$> strOption
  ( long "base-url"
      <> short 'u'
      <> help "API root to run requests against"
      <> value "http://localhost:8080"
  )

fetchParser :: Parser FetchParams
fetchParser = FetchParams <$> startSeason <*> endSeason <*> discipline <*> baseUrl

seasonToCsvParser :: Parser SeasonToCsvParams
seasonToCsvParser = SeasonToCsvParams <$> seasonName <*> discipline <*> baseUrl

progOpts :: ParserInfo ProgramMode
progOpts =
  let
    seasonToCsvMode = SeasonToCsv <$> (seasonToCsvParser <**> helper)
    fetchSeasonsMode = FetchSeasons <$> (fetchParser <**> helper)
  in
    info
      ( hsubparser $
          ( command "season-to-csv" (info seasonToCsvMode (progDesc "write results for a single season to csv"))
              <> command "fetch-seasons-validate"
                ( info fetchSeasonsMode
                    (progDesc "Verify that response parsing succeeds over a range of seasons")
                )
          )
      )
      ( fullDesc
          <> progDesc "Fetch IFSC results to an analysis-friendly format"
          <> header "ifsc-stats - a CLI client for the undocumented IFSC API"
      )
