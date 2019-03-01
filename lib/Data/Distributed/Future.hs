{-# LANGUAGE StaticPointers #-}

module Data.Distributed.Future
  ( Future(..)
  , newEmptyFuture
  , newFuture
  , forkFuture
  , putFuture
  , readFuture
  , isEmptyFuture
  , waitFuture
  , tryReadFuture
  ) where

import Control.Applicative
import Control.Comonad
import Control.Concurrent
-- import Control.Concurrent.MVar.Strict
import Data.Distributive
import System.IO.Unsafe
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.Static



newtype Future a = Future (MVar a)
  deriving (Eq)



instance Static (Typeable a) => Static (Typeable (Future a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Future b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Future b)))
          dict Dict = Dict



newEmptyFuture :: IO (Future a)
newEmptyFuture = Future <$> newEmptyMVar

newFuture :: a -> IO (Future a)
newFuture x = Future <$> newMVar x

forkFuture :: IO a -> IO (Future a)
forkFuture s = do r <- newEmptyFuture
                  _ <- forkIO do x <- s
                                 putFuture r x
                  return r

putFuture :: Future a -> a -> IO ()
putFuture (Future v) = putMVar v

readFuture :: Future a -> IO a
readFuture (Future v) = readMVar v

isEmptyFuture :: Future a -> IO Bool
isEmptyFuture (Future v) = isEmptyMVar v

waitFuture :: Future a -> IO ()
waitFuture (Future v) = do _ <- readMVar v
                           return ()

tryReadFuture :: Future a -> IO (Maybe a)
tryReadFuture (Future v) = tryReadMVar v



instance Foldable Future where
  foldMap f v = f (unsafeDupablePerformIO (readFuture v))

instance Functor Future where
  fmap f v = unsafePerformIO do forkFuture do x <- readFuture v
                                              return (f x)

instance Applicative Future where
  pure x = unsafePerformIO do newFuture x
  liftA2 f v w = unsafePerformIO do forkFuture do x <- readFuture v
                                                  y <- readFuture w
                                                  return (f x y)

-- We don't define this since it is not a "natural" operation
-- instance Traversable Future where
--   traverse :: Applicative f => (a -> f b) -> Future a -> f (Future b)
--   traverse f v = let x = (unsafeDupablePerformIO . readFuture) v
--                      y = f x
--                  in unsafePerformIO . newFuture <$> y

instance Monad Future where
  v >>= f = unsafePerformIO do forkFuture do x <- readFuture v
                                             readFuture (f x)

instance Distributive Future where
  collect :: Functor f => (a -> Future b) -> f a -> Future (f b)
  collect f xs = unsafePerformIO do forkFuture do return ((extract . f) <$> xs)

instance Comonad Future where
  extract v = unsafeDupablePerformIO do readFuture v
  extend f v = unsafePerformIO do forkFuture do return (f v)
