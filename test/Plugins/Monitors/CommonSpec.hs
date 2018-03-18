module Plugins.Monitors.CommonSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Xmobar.Plugins.Monitors.Common

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Common.padString" $ do
    it "returns given string when called with default values" $
      do padString 0 0 "" False "" "test" `shouldBe` "test"

    it "truncates to max width" $ do
      let maxw = 3
          givenStr = "mylongstr"
          expectedStr = take maxw givenStr
      padString 0 maxw "" False "" givenStr `shouldBe` expectedStr

    it "truncates to max width and concatenate with ellipsis" $ do
      let maxw = 3
          givenStr = "mylongstr"
          ellipsis = "..."
          expectedStr = (++ ellipsis) . take 3 $ givenStr
      padString 0 maxw "" False ellipsis givenStr `shouldBe` expectedStr
