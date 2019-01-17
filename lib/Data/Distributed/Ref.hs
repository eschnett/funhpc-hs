{-# LANGUAGE StaticPointers #-}

module Data.Distributed.Ref
 ( Object
 , Ref
 , newRef
 , fetchRef
 , SerializedRef
 , serializeRef
 , deserializeRef
 , local
 , remote
 ) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Foreign
import Foreign.Concurrent as FC
import Type.Reflection

import Control.Distributed.Closure
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.Server
import Data.Distributed.GlobalPtr



-- Layout of an object. Objects are stored via 'GlobalPtr', which
-- internally uses 'StablePtr' to allocate and free.
data Object a = Object { count :: IORef Word
                       , object :: a
                       }

instance (Typeable a, Static (Typeable a)) => Static (Typeable (Object a)) where
  closureDict = closure (static serdict) `cap` closureDict
    where serdict :: Dict (Typeable b) -> Dict (Typeable (Object b))
          serdict Dict = Dict

increfObject :: GlobalPtr (Object a) -> IO ()
increfObject gptr =
  do Just obj <- deRefGlobalPtr gptr
     atomicModifyIORef' (count obj) \cnt -> (cnt + 1, ())

increfObjectC :: ClosureDict (Typeable a)
              -> ClosureDict (Serializable (GlobalPtr (Object a)))
              -> GlobalPtr (Object a) -> Closure (IO ())
increfObjectC cd1 cd2 gptr =
  withClosureDict cd1 $
  closure (static increfObject)
  `cap` cpure cd2 gptr

decrefObject :: GlobalPtr (Object a) -> IO ()
decrefObject gptr =
  do Just obj <- deRefGlobalPtr gptr
     cnt <- atomicModifyIORef' (count obj) \cnt -> (cnt - 1, cnt - 1)
     when (cnt == 0) do freeGlobalPtr gptr

decrefObjectC :: ClosureDict (Typeable a)
              -> ClosureDict (Serializable (GlobalPtr (Object a)))
              -> GlobalPtr (Object a) -> Closure (IO ())
decrefObjectC cd1 cd2 gptr =
  withClosureDict cd1 $
  closure (static decrefObject)
  `cap` cpure cd2 gptr

newObject :: a -> IO (GlobalPtr (Object a))
newObject x =
  do cnt <- newIORef 1 
     obj <- return (Object cnt x)
     gptr <- newGlobalPtr obj
     return gptr
  
newObjectC :: ClosureDict (Serializable a)
           -> a -> Closure (IO (GlobalPtr (Object a)))
newObjectC cd1 x =
  withClosureDict cd1 $
  closure (static newObject)
  `cap` cpure cd1 x

readObject :: GlobalPtr (Object a) -> IO a
readObject gptr =
  do Just obj <- deRefGlobalPtr gptr
     return (object obj)



--------------------------------------------------------------------------------

data Ref a = Ref { globalPtr :: GlobalPtr (Object a)
                 , finalizer :: ForeignPtr ()
                 }
instance Eq (Ref a) where
  Ref gptr1 _ == Ref gptr2 _ = gptr1 == gptr2

incref :: ( Static (Typeable a)
          , Static (Serializable (GlobalPtr (MVar ())))
          , Static (Serializable ())
          , Static (Serializable (GlobalPtr (Object a))))
       => Ref a -> IO ()
incref = increfCD closureDict closureDict closureDict closureDict

increfCD :: ClosureDict (Typeable a)
         -> ClosureDict (Serializable ())
         -> ClosureDict (Serializable (GlobalPtr (MVar ())))
         -> ClosureDict (Serializable (GlobalPtr (Object a)))
         -> Ref a -> IO ()
increfCD cd1 cd2 cd3 cd4 ref =
  lexec do let gptr = globalPtr ref
           mvar <- rcallCD cd2 cd3 (globalPtrRank gptr)
                   (increfObjectC cd1 cd4 gptr)
           takeMVar mvar
           touchForeignPtr (finalizer ref)

decref :: ( Static (Typeable a)
          , Static (Serializable (GlobalPtr (Object a))))
       => Ref a -> IO ()
decref ref = decrefCD closureDict closureDict ref

decrefCD :: ClosureDict (Typeable a)
         -> ClosureDict (Serializable (GlobalPtr (Object a)))
         -> Ref a
         -> IO ()
decrefCD cd1 cd2 ref =
  do let gptr = globalPtr ref
     rexec (globalPtrRank gptr) (decrefObjectC cd1 cd2 gptr)



refFromObject :: ( Static (Typeable a)
                 , Static (Serializable (GlobalPtr (Object a))))
              => GlobalPtr (Object a) -> IO (Ref a)
refFromObject = refFromObjectCD closureDict closureDict

refFromObjectCD :: ClosureDict (Typeable a)
                -> ClosureDict (Serializable (GlobalPtr (Object a)))
                -> GlobalPtr (Object a) -> IO (Ref a)
refFromObjectCD cd1 cd2 gptr =
  do ptr <- malloc
     fptr <- newForeignPtr_ ptr
     let ref = Ref gptr fptr
     let fin = do decrefCD cd1 cd2 ref
                  free ptr
     FC.addForeignPtrFinalizer fptr fin
     return ref

newRef :: ( Static (Typeable a)
          , Static (Serializable (GlobalPtr (Object a))))
       => a -> IO (Ref a)
newRef = newRefCD closureDict closureDict

newRefCD :: ClosureDict (Typeable a)
         -> ClosureDict (Serializable (GlobalPtr (Object a)))
         -> a -> IO (Ref a)
newRefCD cd1 cd2 x =
  do gptr <- newObject x
     ref <- refFromObjectCD cd1 cd2 gptr
     return ref

fetchRef :: Ref a -> IO a
fetchRef (Ref gptr _) =
  -- TODO: Check rank
  readObject gptr



--------------------------------------------------------------------------------

newtype SerializedRef a = SerializedRef (GlobalPtr (Object a))
  deriving (Eq, Binary)

serializeRef :: ( Static (Typeable a)
                , Static (Serializable ())
                , Static (Typeable (MVar ()))
                , Static (Serializable (GlobalPtr (Object a))))
             => Ref a -> IO (SerializedRef a)
serializeRef ref =
  do let gptr = globalPtr ref
     incref ref
     return (SerializedRef gptr)

deserializeRef :: ( Static (Typeable a)
                  , Static (Serializable (GlobalPtr (Object a))))
               => SerializedRef a -> IO (Ref a)
deserializeRef (SerializedRef gptr) =
  do ref <- refFromObject gptr
     decref ref
     return ref



--------------------------------------------------------------------------------

execLocal :: ( Static (Typeable a)
             , Static (Serializable (GlobalPtr (Object a))))
          => IO a -> IO (Ref a)
execLocal = execLocalCD closureDict closureDict

execLocalCD :: ClosureDict (Typeable a)
            -> ClosureDict (Serializable (GlobalPtr (Object a)))
            -> IO a -> IO (Ref a)
execLocalCD cd1 cd2 act =
  do x <- act
     ref <- newRefCD cd1 cd2 x
     return ref

execLocalC :: ClosureDict (Typeable a)
           -> ClosureDict (Serializable (GlobalPtr (Object a)))
           -> Closure (IO a) -> Closure (IO (Ref a))
execLocalC cd1 cd2 cl =
  withClosureDict cd1 $
  closure (static execLocalCD)
  `cap` cduplicate cd1
  `cap` cduplicate cd2
  `cap` cl



local :: ( Static (Typeable a)
         , Static (Serializable (GlobalPtr (Object a))))
      => IO a -> IO (MVar (Ref a))
local = localCD closureDict closureDict

localCD :: ClosureDict (Typeable a)
        -> ClosureDict (Serializable (GlobalPtr (Object a)))
        -> IO a -> IO (MVar (Ref a))
localCD cd1 cd2 act =
  do mvar <- newEmptyMVar
     _ <- forkIO do ref <- execLocalCD cd1 cd2 act
                    putMVar mvar ref
     return mvar



remote :: ( Static (Typeable a)
          , Static (Serializable (GlobalPtr (Object a)))
          , Static (Serializable (GlobalPtr (MVar (Ref a))))
          , Static (Serializable (Ref a)))
       => MPI.Rank -> Closure (IO a) -> IO (MVar (Ref a))
remote = remoteCD closureDict closureDict closureDict closureDict

remoteCD :: ClosureDict (Serializable (Ref a))
         -> ClosureDict (Serializable (GlobalPtr (MVar (Ref a))))
         -> ClosureDict (Typeable a)
         -> ClosureDict (Serializable (GlobalPtr (Object a)))
         -> MPI.Rank -> Closure (IO a) -> IO (MVar (Ref a))
remoteCD cd1 cd2 cd3 cd4 rank act =
  rcallCD cd1 cd2 rank (execLocalC cd3 cd4 act)
