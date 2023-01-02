module Main where

import Prelude

import Affjax (printError)
import Cli.Parser (EndSeason(..), FetchParams(..), StartSeason(..), progOpts)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (stringify)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Options.Applicative (execParser)
import Web.IFSC.Client (FetchError(..), fullSeasons)
import Web.IFSC.Model (SeasonName(..))

main :: Effect Unit
main = do
  (FetchParams (StartSeason (SeasonName start)) (EndSeason (SeasonName end)) discipline baseUrl) <- execParser progOpts
  launchAff_ $ (runExceptT $ runReaderT (fullSeasons discipline (Just start) (Just end)) baseUrl) >>= case _ of
    Right _ -> log "fetch successful"
    Left (FetchError e url body) ->
      log $
        intercalate "\n" [ "=======", "At url: " <> url, printError e, "----", stringify body ]

