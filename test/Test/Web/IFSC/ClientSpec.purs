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
import Web.IFSC.Client (BaseUrl(..), getLandingPage)

spec :: Spec Unit
spec =
  describe "Client functions"
    $ do
        it "should fetch the landing page successfully"
          $ do
              liftAff testLandingPage

-- what do I want to tesT?
-- making a request to the url on the wiremock server
-- results in decoding successfully?
-- results in the specific data?
-- can can encode <-> decode?
adaptError :: forall a. Either Error a -> Either String a
adaptError = lmap printError

testLandingPage :: Aff Unit
testLandingPage = 
  let
    test = do
       landingPageResult <- getLandingPage
       let
         adapted = adaptError landingPageResult
       adapted `shouldSatisfy` isRight
  in runReaderT test (BaseUrl "http://localhost:8080") 
