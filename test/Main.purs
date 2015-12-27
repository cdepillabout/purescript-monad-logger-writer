module Test.Main where

import Prelude
    ( class Eq, class Show, Unit, bind, ($), (<<<), pure, (<>), (>>=), map
    , show, id, (+), (<$>), const, return
    )

import Data.Functor (($>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, readRef, writeRef, newRef)
import Control.Monad.Writer.Class (listen, tell, writer)
import Data.Foldable (sequence_)
import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Test.Spec (describe, it)
import Test.Spec.Runner (Process, run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Control.Monad.Logger (Logger(..))

runLoggerEff :: forall eff w a
              . Logger eff w a
             -> w
             -> Aff (ref :: REF | eff) (Tuple a w)
runLoggerEff (Logger logger) w = do
    ref <- liftEff $ newRef w
    a <- liftEff $ logger ref
    w' <- liftEff $ readRef ref
    return $ Tuple a w

loggerIgnoreRef :: forall eff w . Logger eff w Int
loggerIgnoreRef = Logger <<< const $ pure 100

loggerOverwriteRef :: forall eff . Logger eff Int Int
loggerOverwriteRef = Logger \ref -> writeRef ref 1000 $> 100

loggerReadRef :: forall eff w . Logger eff w w
loggerReadRef = Logger readRef

loggerWriteNewRef :: forall eff . Logger eff Int Int
loggerWriteNewRef = Logger \ref -> do
    a <- readRef ref
    writeRef ref 1000
    pure a

testWithLogger :: forall eff
                . (Logger eff Int Int -> Logger eff Int Int)
               -> (Logger eff Int Int -> Logger eff Int Int)
               -> Logger eff Int Int
               -> Aff (ref :: REF | eff) Unit
testWithLogger exA exB logger = do
    resA <- runLoggerEff (exA logger) 10
    resB <- runLoggerEff (exB logger) 10
    resA `shouldEqual` resB

testWithAllLoggers :: forall eff
                    . (Logger eff Int Int -> Logger eff Int Int)
                   -> (Logger eff Int Int -> Logger eff Int Int)
                   -> Aff (ref :: REF | eff) Unit
testWithAllLoggers exA exB = do
    let test = testWithLogger exA exB
    test loggerIgnoreRef
    test loggerOverwriteRef
    test loggerReadRef
    test loggerWriteNewRef

testLoggers :: forall eff
             . Logger eff Int Int
            -> Logger eff Int Int
            -> Aff (ref :: REF | eff) Unit
testLoggers = testLoggers' id

testLoggers' :: forall eff w a
              . (Show w, Show a, Eq a, Eq w)
             => (Int -> w)
             -> Logger eff w a
             -> Logger eff w a
             -> Aff (ref :: REF | eff) Unit
testLoggers' constructor loggerA loggerB = do
    resA <- runLoggerEff loggerA $ constructor 10
    resB <- runLoggerEff loggerB $ constructor 10
    resA `shouldEqual` resB


loggerMonadF :: forall eff . Int -> Logger eff Int Int
loggerMonadF a = pure a

loggerMonadG :: forall eff . Int -> Logger eff Int Int
loggerMonadG _ = pure 100

loggerMonadH :: forall eff . Int -> Logger eff Int Int
loggerMonadH a = Logger $ \ref -> writeRef ref a $> 100

loggerMonadI :: forall eff . Int -> Logger eff Int Int
loggerMonadI a = Logger $ \ref -> readRef ref >>= \b -> writeRef ref a $> b

loggerMonadJ :: forall eff . Int -> Logger eff Int Int
loggerMonadJ _ = Logger $ readRef

loggerMonadK :: forall eff . Int -> Logger eff Int Int
loggerMonadK _ = Logger $ \ref -> (+ 10000) <$> readRef ref

allCombinations :: forall eff . Array (Aff (ref :: REF | eff) Unit)
allCombinations = do
    m <- [loggerIgnoreRef, loggerOverwriteRef, loggerReadRef, loggerWriteNewRef]
    f <- [loggerMonadF, loggerMonadG, loggerMonadH, loggerMonadI, loggerMonadJ, loggerMonadK]
    g <- [loggerMonadF, loggerMonadG, loggerMonadH, loggerMonadI, loggerMonadJ, loggerMonadK]
    pure $ testLoggers ((m >>= f) >>= g) (m >>= (\x -> f x >>= g))

main :: forall eff . Eff (process :: Process, console :: CONSOLE, random :: RANDOM, err :: EXCEPTION, ref :: REF | eff) Unit
main = run [consoleReporter] do
    describe "Control.Monad.Logger" do
        describe "follows functor laws" $ do
            it "map id === id" $ do
                testWithAllLoggers (map id) id
                1 `shouldEqual` 1

            it "map (p <<< q) === map p <<< map q" $ do
                let p = length
                    q = show
                    equationA = map $ p <<< q
                    equationB = map p <<< map q
                testWithAllLoggers equationA equationB

        describe "follows monad laws" $ do
            it "left identity: pure a >>= f === f a" $ do
                testLoggers (pure 10 >>= loggerMonadF) (loggerMonadF 10)
                testLoggers (pure 10 >>= loggerMonadG) (loggerMonadG 10)
                testLoggers (pure 10 >>= loggerMonadH) (loggerMonadH 10)
                testLoggers (pure 10 >>= loggerMonadI) (loggerMonadI 10)
                testLoggers (pure 10 >>= loggerMonadJ) (loggerMonadJ 10)
                testLoggers (pure 10 >>= loggerMonadK) (loggerMonadK 10)

            it "right identity: m >>= pure === m" $ do
                testLoggers (loggerIgnoreRef    >>= pure) loggerIgnoreRef
                testLoggers (loggerOverwriteRef >>= pure) loggerOverwriteRef
                testLoggers (loggerReadRef      >>= pure) loggerReadRef
                testLoggers (loggerWriteNewRef  >>= pure) loggerWriteNewRef

            it "associativity: (m >>= f) >>= g === m >>= (\\x -> f x >>= g)" $ do
                sequence_ allCombinations

        describe "follows monadwriter laws" $ do
            it "writer (a, mempty) === pure a" $ do
                let lhs = writer $ Tuple (Additive 100) (mempty :: Additive Int)
                    rhs = pure $ Additive 100
                testLoggers' Additive lhs rhs

            it "tell x *> tell y === tell (x <> y)" $ do
                let lhs = tell (Additive 100) *> tell (Additive 1000)
                    rhs = tell ((Additive 100) <> (Additive 1000))
                testLoggers' Additive lhs rhs

            it "listen (pure x) === pure (a, mempty)" $ do
                let lhs = listen <<< pure $ Additive 100
                    rhs = pure $ Tuple (Additive 100) (mempty :: Additive Int)
                testLoggers' Additive lhs rhs

            it "listen (writer (a, x)) === tell x $> Tuple a x" $ do
                let a = Additive 100
                    x = Additive 1000
                    lhs = listen <<< writer $ Tuple a x
                    rhs = tell x $> Tuple a x
                testLoggers' Additive lhs rhs
