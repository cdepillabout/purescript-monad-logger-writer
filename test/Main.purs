module Test.Main where

import Prelude

import Data.Functor (($>))
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, Ref, readRef, writeRef, newRef)
import Data.Monoid (class Monoid)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Spec (describe, pending, it, prop)
import Test.Spec.Runner (Process, run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Logger (Logger(..), runLogger')

runLoggerEff :: forall eff w a
              . Logger eff w a
             -> w
             -> Aff (ref :: REF | eff) (Tuple a w)
runLoggerEff (Logger logger) w = do
    ref <- liftEff $ newRef w
    a <- liftEff $ logger ref
    w' <- liftEff $ readRef ref
    return $ Tuple a w

main :: forall eff . Eff (process :: Process, console :: CONSOLE, random :: RANDOM, err :: EXCEPTION, ref :: REF | eff) Unit
main = run [consoleReporter] do
    describe "Control.Monad.Logger" do
        describe "follows functor laws" $ do
            it "map id = id" $ do
                let logger1 = Logger \ref -> writeRef ref 10 $> 3
                res1 <- runLoggerEff (map id logger1) 4
                res2 <- runLoggerEff logger1 4
                res1 `shouldEqual` res2

                let logger2 = Logger \ref -> pure 3
                res3 <- runLoggerEff (map id logger2) 5
                res4 <- runLoggerEff logger2 5
                res3 `shouldEqual` res4

            it "map (p <<< q) = map p <<< map q" $ do
                let p = length
                    q = show
                let logger1 = Logger \ref -> writeRef ref 1000 $> 100
                res1 <- runLoggerEff (map (p <<< q) logger1) 10
                res2 <- runLoggerEff ((map p <<< map q) logger1) 10
                res1 `shouldEqual` res2

                let logger2 = Logger \ref -> pure 100
                res3 <- runLoggerEff (map (p <<< q) logger2) 10
                res4 <- runLoggerEff ((map p <<< map q) logger2) 10
                res3 `shouldEqual` res4
