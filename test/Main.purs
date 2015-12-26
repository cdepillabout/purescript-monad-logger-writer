module Test.Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, Ref)
import Data.Monoid (class Monoid)
import Data.Tuple (Tuple)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Spec (describe, pending, it, prop)
import Test.Spec.Runner (Process, run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Logger (Logger(..), runLogger')

runLoggerEff :: forall eff w a . (Monoid w) => Logger eff w a -> Aff (ref :: REF | eff) (Tuple a w)
runLoggerEff = liftEff <<< runLogger'

main :: forall eff . Eff (process :: Process, console :: CONSOLE, random :: RANDOM, err :: EXCEPTION, ref :: REF | eff) Unit
main = run [consoleReporter] do
    describe "What is it?" do
        it "awesome" do
            let isAwesome = true
            isAwesome `shouldEqual` true

    describe "Control.Monad.Logger" do
        it "follows functor law id" $ do
            let logger = Logger \(ref :: Ref String) -> return (3 :: Int)
            res1 <- runLoggerEff (map id logger)
            res2 <- runLoggerEff logger
            res1 `shouldEqual` res2
