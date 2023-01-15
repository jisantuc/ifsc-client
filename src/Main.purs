module Main where

import Prelude

import Affjax (printError)
import Cli.Parser (EndSeason(..), FetchParams(..), ProgramMode(..), SeasonToCsvParams(..), StartSeason(..), progOpts)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (stringify)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeFile)
import Node.Path (FilePath)
import Options.Applicative (execParser)
import Web.IFSC.Client (FetchError(..), analysisResultsForSeason, fullSeasons)
import Web.IFSC.Model (SeasonName(..), toCsv)

main :: Effect Unit
main = do
  programMode <- execParser progOpts
  launchAff_ $ case programMode of
    FetchSeasons (FetchParams (StartSeason (SeasonName start)) (EndSeason (SeasonName end)) discipline baseUrl) ->
      (runExceptT $ runReaderT (fullSeasons discipline (Just start) (Just end)) baseUrl) >>= case _ of
        Right _ -> log "fetch successful"
        Left (FetchError e url body) ->
          log $
            intercalate "\n" [ "=======", "At url: " <> url, printError e, "----", stringify body ]
    SeasonToCsv (SeasonToCsvParams seasonName discipline baseUrl) ->
      (runExceptT $ runReaderT (analysisResultsForSeason seasonName discipline) baseUrl) >>= case _ of
        Right results ->
          let
            csvOut = toCsv results

            outputPath :: FilePath
            outputPath = show seasonName <> ".csv"
          in
            liftEffect (Buffer.fromString csvOut UTF8) >>= writeFile outputPath
        Left (FetchError e url body) ->
          log $
            intercalate "\n" [ "=======", "At url: " <> url, printError e, "----", stringify body ]

