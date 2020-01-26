module Test.Main where

import Prelude
import Data.Array ((:))
import Data.Date.Component (Month(..))
import Data.DateTime (canonicalDate, Date)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Main (Index(..), Today(..), Ttl(..), deleteExpiredIndices)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Time.Duration (Days(..))
import Data.Int (toNumber)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Main" do
          describe "deleteExpiredIndices" do
            it "should delete expired indices" do
              result <- liftEffect $ Ref.new []
              let
                expected =
                  (map show)
                    [ Index "foo_2019_11_15"
                    , Index "foo_2020_01_06"
                    ]
              deleteExpiredIndices
                (client result)
                (Today $ date 2020 January 10)
                (Ttl $ Days $ toNumber 3)
              actual <- liftEffect $ (map show) <$> Ref.read result
              actual `shouldEqual` expected
  where
  client result =
    { indices:
      pure
        [ Index "foo_2020_01_11"
        , Index "foo_2020_01_10"
        , Index "foo_2020_01_09"
        , Index "foo_2020_01_08"
        , Index "foo_2020_01_07"
        , Index "foo_2020_01_06"
        , Index "foo_2019_11_15"
        , Index "index_without_date"
        ]
    , deleteIndex: \index -> liftEffect $ Ref.modify_ ((:) index) result :: Aff Unit
    }

  date :: Int -> Month -> Int -> Date
  date year month day = unsafePartial fromJust $ (\y d -> canonicalDate y month d) <$> toEnum year <*> toEnum day
