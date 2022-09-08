module Test.Web.IFSC.ModelSpec
  ( spec
  ) where

import Test.Spec
import Prelude (Unit, ($))
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec =
  describe "Test suite" do
    it "has a test" $ fail "oh no"
