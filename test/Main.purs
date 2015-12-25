module Test.Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE)
import Test.Spec (describe, pending, it)
import Test.Spec.Runner (Process, run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

main :: forall eff . Eff (process :: Process, console :: CONSOLE | eff) Unit
main = run [consoleReporter] do
  describe "purescript-spec" do
    describe "What is it?" do
      it "awesome" do
        let isAwesome = true
        isAwesome `shouldEqual` true
    describe "Features" do
      it "runs in NodeJS" $ return unit
      it "runs in the browser" $ return unit
      it "supports async specs" do
        res <- later' 100 $ return "Alligator"
        res `shouldEqual` "Alligator"
      it "is PureScript 0.8 compatible" $ return unit
