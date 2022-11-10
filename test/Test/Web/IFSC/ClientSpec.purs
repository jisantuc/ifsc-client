module Test.Web.IFSC.ClientSpec where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, isRight)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Web.IFSC.Client (getLandingPage)

spec :: Spec Unit
spec =
  describe "Client functions" $ do
     it "should fetch the landing page successfully" $ do
        liftAff testLandingPage
--   describe "Case-insensitive strings" do
--     it "Decodes CI-variants of required strings" $ do

-- what do I want to tesT?
-- making a request to the url on the wiremock server
-- results in decoding successfully?
-- results in the specific data?
-- can can encode <-> decode?

adaptError :: forall a. Either Error a -> Either String a
adaptError = lmap printError

testLandingPage :: Aff Unit
testLandingPage =  do
  landingPageResult <- getLandingPage 
  let adapted = adaptError landingPageResult
  adapted `shouldSatisfy` isRight
