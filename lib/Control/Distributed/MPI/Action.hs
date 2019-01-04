module Control.Distributed.MPI.Action
  ( registerEntry
  , lookupEntry
  , Dict
  , dict

  , Action(..)
  , SomeAction(..)
  , makeAction
  , registerAction
  ) where

import Prelude hiding (lookup)

import Data.IORef
import Data.Map.Strict
import GHC.Fingerprint
import System.IO.Unsafe
import Type.Reflection
import Type.Reflection.Unsafe

import Data.Binary



-- These are essentially global dictionaries for type classes

registerEntry :: Ord k => IORef (Map k a) -> k -> a -> IO ()
registerEntry xs k x = atomicModifyIORef' xs $ \xs' -> (insert k x xs', ())

lookupEntry :: Ord k => IORef (Map k a) -> k -> a
lookupEntry xs k = unsafePerformIO (readIORef xs) ! k

type Dict k a = IORef (Map k a)

dict :: Dict k a
dict = unsafePerformIO (newIORef empty)



-- A function returning an I/O action
class Action a where
  run :: a -> IO ()

data SomeAction where
  SomeAction :: forall a. (Action a, Binary a, Typeable a)
             => TypeRep a -> a -> SomeAction

makeAction :: (Action a, Binary a, Typeable a) => a -> SomeAction
makeAction act = SomeAction typeRep act

instance Action SomeAction where
  run (SomeAction _ act) = run act

instance Binary SomeAction where
  put (SomeAction tx x) = put (typeRepFingerprint tx) >> put x
  get = do fp <- get
           lookupEntry getSomeAction fp

registerAction :: forall a. (Action a, Binary a, Typeable a) => IO ()
registerAction =
  registerEntry getSomeAction fp (makeAction <$> get @a)
  where fp = typeRepFingerprint (typeRep @a)

getSomeAction :: Dict Fingerprint (Get SomeAction)
getSomeAction = dict
