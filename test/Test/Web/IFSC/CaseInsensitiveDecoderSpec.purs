module Test.Web.IFSC.CaseInsensitiveDecoderSpec  where

import Prelude

import Data.Argonaut (class DecodeJson, decodeJson, encodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.String as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen as G
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Web.IFSC.Model (CompetitionCategory(..), Discipline(..), RoundName(..))

spec :: Spec Unit
spec =
  describe "Case-insensitive strings" do
    it "Decodes CI-variants of required strings" $ do
      quickCheck testDisciplineCodec
      quickCheck testCompetitionCategoryCodec
      quickCheck testRoundNameCodec

genCIString :: String -> G.Gen String
genCIString s =
  let
    chars = S.singleton <$> S.toCodePointArray s
  in
    fold
      <$> traverse
          ( \c ->
              ( case _ of
                  codecPair
                    | codecPair 
                    >= 0.67 -> S.toUpper c
                  codecPair

                    | codecPair 
                    >= 0.33 -> S.toLower c
                  _ -> c
              )
                <$> G.uniform
          )
          chars

testDisciplineCodec ∷ DisciplineCodecPair → Boolean
testDisciplineCodec (DisciplineCodecPair tup) = testCodec' tup

testCompetitionCategoryCodec :: CompetitionCategoryCodecPair -> Boolean
testCompetitionCategoryCodec (CompetitionCategoryCodecPair tup) = testCodec' tup

testRoundNameCodec :: RoundNameCodecPair -> Boolean
testRoundNameCodec (RoundNameCodecPair tup) = testCodec' tup

testCodec' :: forall expectationType. DecodeJson expectationType => Eq expectationType => Tuple String expectationType  -> Boolean
testCodec' (Tuple inString expectation) =
  ((decodeJson <<< encodeJson) inString) == (Right expectation)

newtype CIString = CIString String

derive newtype instance Eq CIString

derive newtype instance Show CIString

newtype DisciplineCodecPair = DisciplineCodecPair (Tuple String Discipline)

instance Arbitrary DisciplineCodecPair where
  arbitrary = 
    let elems = NE.appendArray (NE.singleton $ Tuple "speed" Speed) [
        Tuple "lead" Lead,
        Tuple "boulder" Boulder,
        Tuple "combined" Combined
      ]
    in
      arbitraryCodecPair DisciplineCodecPair elems

newtype CompetitionCategoryCodecPair = CompetitionCategoryCodecPair (Tuple String CompetitionCategory)

instance Arbitrary CompetitionCategoryCodecPair where
  arbitrary =
    let elems = NE.appendArray (NE.singleton $ Tuple "men" Men) [
        Tuple "women" Women
      ]
    in
      arbitraryCodecPair CompetitionCategoryCodecPair elems

newtype RoundNameCodecPair = RoundNameCodecPair (Tuple String RoundName)

instance Arbitrary RoundNameCodecPair where
  arbitrary =
    let elems = NE.appendArray (NE.singleton $ Tuple "qualification" Qualification) [
        Tuple "semifinal" SemiFinal
        , Tuple "final" Final
      ]
    in
      arbitraryCodecPair RoundNameCodecPair elems
  
arbitraryCodecPair :: forall expectationType codecPair.
  Show expectationType =>
 (Tuple String expectationType -> codecPair)
 -> NonEmptyArray (Tuple String expectationType) -> G.Gen codecPair
arbitraryCodecPair constructor tupleNonEmptyArray =
      constructor <$> do
        withCIString <- traverse (\(Tuple inString expectation) ->
          (\x -> Tuple x expectation) <$> genCIString inString
        ) tupleNonEmptyArray
        G.elements withCIString