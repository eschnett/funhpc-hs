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
import Data.IORef
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
                  forkIO_ do s >>= putFuture r
                  return r
--DEBUG threads :: IORef [ThreadId]
--DEBUG threads = unsafePerformIO $ newIORef []
forkIO_ :: IO () -> IO ()
--DEBUG forkIO_ s = do tid <- forkIO s
--DEBUG                atomicModifyIORef' threads \tids -> (tid:tids, ())
--DEBUG                return ()
forkIO_ s = do _ <- forkIO s
               return ()

putFuture :: Future a -> a -> IO ()
putFuture (Future v) = putMVar v

readFuture :: Future a -> IO a
readFuture (Future v) = readMVar v

isEmptyFuture :: Future a -> IO Bool
isEmptyFuture (Future v) = isEmptyMVar v

waitFuture :: Future a -> IO ()
waitFuture (Future v) = readMVar v >> return ()

tryReadFuture :: Future a -> IO (Maybe a)
tryReadFuture (Future v) = tryReadMVar v



--TODO instance Foldable Future where
--TODO   foldMap f v = f (unsafeDupablePerformIO (readFuture v))
--TODO 
--TODO instance Functor Future where
--TODO   fmap f v = unsafePerformIO do forkFuture do x <- readFuture v
--TODO                                               return (f x)
--TODO 
--TODO instance Applicative Future where
--TODO   pure x = unsafePerformIO do newFuture x
--TODO   liftA2 f v w = unsafePerformIO do forkFuture do x <- readFuture v
--TODO                                                   y <- readFuture w
--TODO                                                   return (f x y)
--TODO 
--TODO -- We don't define this since it is not a "natural" operation
--TODO -- instance Traversable Future where
--TODO --   traverse :: Applicative f => (a -> f b) -> Future a -> f (Future b)
--TODO --   traverse f v = let x = (unsafeDupablePerformIO . readFuture) v
--TODO --                      y = f x
--TODO --                  in unsafePerformIO . newFuture <$> y
--TODO 
--TODO instance Monad Future where
--TODO   v >>= f = unsafePerformIO do forkFuture do x <- readFuture v
--TODO                                              readFuture (f x)
--TODO 
--TODO instance Distributive Future where
--TODO   collect :: Functor f => (a -> Future b) -> f a -> Future (f b)
--TODO   collect f xs = unsafePerformIO do forkFuture do return ((extract . f) <$> xs)
--TODO 
--TODO instance Comonad Future where
--TODO   extract v = unsafeDupablePerformIO do readFuture v
--TODO   extend f v = unsafePerformIO do forkFuture do return (f v)
