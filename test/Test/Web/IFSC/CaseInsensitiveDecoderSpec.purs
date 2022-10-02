module Test.Web.IFSC.CaseInsensitiveDecoderSpec
  ( CompetitionCategoryCodecPair(..)
  , DisciplineCodecPair(..)
  , genCIString
  , spec
  , testDisciplineCodec
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.String (toLower, toUpper)
import Data.String as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as G
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Web.IFSC.Model (CompetitionCategory(..), Discipline(..))

spec :: Spec Unit
spec =
  describe "Case-insensitive strings" do
    it "Decodes CI-variants of required strings" $ do
      quickCheck testDisciplineCodec
      quickCheck testCompetitionCategoryCodec

genCIString :: String -> G.Gen String
genCIString s =
  let
    chars = S.singleton <$> S.toCodePointArray s
  in
    fold
      <$> traverse
          ( \c ->
              ( \x -> case mod x 1.0 of
                  d
                    | d >= 0.67 -> toUpper c
                  d
                    | d >= 0.33 -> toLower c
                  _ -> c
              )
                <$> arbitrary
          )
          chars

testDisciplineCodec ∷ DisciplineCodecPair → Boolean
testDisciplineCodec (DisciplineCodecPair tup) = testCodec' tup

testCompetitionCategoryCodec :: CompetitionCategoryCodecPair -> Boolean
testCompetitionCategoryCodec (CompetitionCategoryCodecPair tup) = testCodec' tup

testCodec' :: forall a. DecodeJson a => Eq a => Tuple String a  -> Boolean
testCodec' (Tuple inString expectation) =
  ((decodeJson <<< encodeJson) inString) == (Right expectation)

newtype DisciplineCodecPair = DisciplineCodecPair (Tuple String Discipline)

instance Arbitrary DisciplineCodecPair where
  arbitrary = 
      (G.elements 
        $ DisciplineCodecPair
        <$> NE.appendArray (NE.singleton $ Tuple "speed" Speed) [
        Tuple "lead" Lead,
        Tuple "boulder" Boulder,
        Tuple "combined" Combined
      ])
      >>= (\(DisciplineCodecPair (Tuple inString expectation)) ->
        (\x -> DisciplineCodecPair (Tuple x expectation)) <$> genCIString inString
      )

newtype CompetitionCategoryCodecPair = CompetitionCategoryCodecPair (Tuple String CompetitionCategory)

instance Arbitrary CompetitionCategoryCodecPair where
  arbitrary =
    G.elements $
      CompetitionCategoryCodecPair
      <$> NE.appendArray (NE.singleton $ Tuple "men" Men) [
        Tuple "women" Women
      ]