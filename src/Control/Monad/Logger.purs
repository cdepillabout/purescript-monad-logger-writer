
module Control.Monad.Logger where

import Prelude
    ( class Monad, class Bind, class Apply, class Applicative, class Functor
    , bind, (>>=), (<*>), pure, const, (<<<), map, return, ($)
    )

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, readRef, newRef)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

newtype Logger eff w a = Logger (Ref w -> Eff (ref :: REF | eff) a)

runLogger :: forall eff w a . Logger eff w a -> Ref w -> Eff (ref :: REF | eff) a
runLogger (Logger logger) = logger

-- | Run a Logger computation, starting with an empty log.
runLogger' :: forall eff w a . (Monoid w) => Logger eff w a -> Eff (ref :: REF | eff) (Tuple a w)
runLogger' (Logger logger) = do
    ref <- newRef mempty
    a <- logger ref
    w <- readRef ref
    return $ Tuple a w

instance functorLogger :: Functor (Logger eff w) where
    map f (Logger logger) = Logger \ref -> map f (logger ref)

instance applicativeLogger :: Applicative (Logger eff w) where
    pure = Logger <<< const <<< pure

instance applyLogger :: Apply (Logger eff w) where
    apply (Logger f) (Logger v) = Logger \ref -> f ref <*> v ref

instance bindLogger :: Bind (Logger eff w) where
    bind (Logger v) f = Logger \ref -> v ref >>= \a -> runLogger (f a) ref

instance monadLogger :: Monad (Logger eff w)

-- instance (Monoid w) => Monad (Logger w) where
--   return = pure
--   Logger l >>= f = Logger $ \r -> l r >>= \a -> runLogger (f a) r

-- instance (Monoid w) => MonadIO (Logger w) where
--   liftIO = Logger . const

-- instance (Monoid w) => MonadWriter w (Logger w) where
--   tell w = Logger $ \r -> atomicModifyIORef' r $ \w' -> (mappend w' w, ())
--   listen l = Logger $ \r -> do
--     (a, w) <- liftIO (runLogger' l)
--     atomicModifyIORef' r $ \w' -> (mappend w' w, (a, w))
--   pass l = Logger $ \r -> do
--     ((a, f), w) <- liftIO (runLogger' l)
--     atomicModifyIORef' r $ \w' -> (mappend w' (f w), a)

