module Web.IFSC.Model where

import Data.Date (Date)
import Data.Tuple (Tuple)

type LandingPage
  = { id :: SeasonId
    , name :: SeasonName
    , url :: String
    , disciplineKinds :: Array (Tuple Int Discipline)
    , leagues :: Array League
    }

newtype SeasonId
  = SeasonId Int

newtype SeasonName
  = SeasonName String

data Discipline
  = Speed
  | Lead
  | Boulder
  | Combined

type League
  = { id :: Int
    , name :: LeagueName
    }

newtype LeagueName
  = LeagueName String

newtype LeagueId
  = LeagueId Int

worldCupsAndWorldChampionships :: LeagueName
worldCupsAndWorldChampionships = LeagueName "World Cups and World Championships"

type Event
  = { localStartDate :: Date
    , localEndDate :: Date
    , event :: String
    , url :: String
    }

type EventResults
  = { category :: CompetitionCategory
    , discipline :: Discipline
    , fullResultsUrl :: String
    }

data CompetitionCategory
  = Men
  | Women

type EventFullResults
  = { ranking :: Array CompetitorResult
    }

type CompetitorResult
  = { firstname :: String
    , lastName :: String
    , rounds :: Array Round
    }

type Round
  = { roundName :: RoundName
    , score :: ScoreString
    , ascents :: Array Ascent
    }

data RoundName
  = Qualification
  | SemiFinal
  | Final

newtype ScoreString
  = ScoreString String

type Ascent
  = { top :: Boolean
    , zone :: Boolean
    , topTries :: Int
    , zoneTries :: Int
    }
