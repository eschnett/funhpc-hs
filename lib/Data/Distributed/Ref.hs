{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

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
import GHC.Generics
import System.IO.Unsafe
import Type.Reflection

import Control.Distributed.Closure
import Control.Distributed.Closure.Instances()
import Control.Distributed.Closure.StaticT
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.Server
import Control.Distributed.MPI.World
import Data.Distributed.GlobalPtr



-- Layout of an object. Objects are stored via 'GlobalPtr', which
-- internally uses 'StablePtr' to allocate and free.
data Object a = Object { count :: IORef Word
                       , object :: a
                       }

instance StaticT (Typeable a) (Typeable (Object a)) where
  closureDictT cd = withClosureDict cd $ closure (static \Dict -> Dict) `cap` cd

increfObject :: GlobalPtr (Object a) -> IO ()
increfObject gptr =
  do mobj <- deRefGlobalPtr gptr
     let Just obj = mobj
     newcnt <- atomicModifyIORef' (count obj) \cnt -> (cnt + 1, cnt + 1)
     -- putStrLn $ "incref newcnt=" ++ show newcnt
     return ()

increfObjectC :: forall a. ClosureDict (Typeable a)
              -> GlobalPtr (Object a) -> Closure (IO ())
increfObjectC cd gptr =
  withClosureDict cd
  let cd2 = closureDictT cd :: ClosureDict (Typeable (Object a))
      cd3 = closureDictT cd2 :: ClosureDict (Binary (GlobalPtr (Object a)))
      cd4 = closureDictT cd2 :: ClosureDict (Typeable (GlobalPtr (Object a)))
      cd5 = combineClosureDict cd3 cd4
  in closure (static increfObject)
     `cap` cpure cd5 gptr

decrefObject :: GlobalPtr (Object a) -> IO ()
decrefObject gptr =
  do mobj <- deRefGlobalPtr gptr
     let Just obj = mobj
     newcnt <- atomicModifyIORef' (count obj) \cnt -> (cnt - 1, cnt - 1)
     -- putStrLn $ "decref newcnt=" ++ show newcnt
     when (newcnt == 0) do freeGlobalPtr gptr
     return ()

decrefObjectC :: forall a. ClosureDict (Typeable a)
              -> GlobalPtr (Object a) -> Closure (IO ())
decrefObjectC cd gptr =
  withClosureDict cd
  let cd2 = closureDictT cd :: ClosureDict (Typeable (Object a))
      cd3 = closureDictT cd2 :: ClosureDict (Binary (GlobalPtr (Object a)))
      cd4 = closureDictT cd2 :: ClosureDict (Typeable (GlobalPtr (Object a)))
      cd5 = combineClosureDict cd3 cd4
  in closure (static decrefObject)
     `cap` cpure cd5 gptr

newObject :: a -> IO (GlobalPtr (Object a))
newObject x =
  do cnt <- newIORef 1 
     obj <- return (Object cnt x)
     -- putStrLn "newobj cnt=1"
     gptr <- newGlobalPtr obj
     return gptr
  
newObjectC :: ClosureDict (Serializable a)
           -> a -> Closure (IO (GlobalPtr (Object a)))
newObjectC cd x =
  withClosureDict cd $
  closure (static newObject)
  `cap` cpure cd x



--------------------------------------------------------------------------------

data Ref a = Ref { typeableDict :: ClosureDict (Typeable a)
                 , globalPtr :: GlobalPtr (Object a)
                 , finalizer :: ForeignPtr ()
                 }
  deriving (Generic)

instance Eq (Ref a) where
  ref1 == ref2 = globalPtr ref1 == globalPtr ref2

incref :: Ref a -> IO ()
incref ref =
  lexec do let gptr = globalPtr ref
           mvar <- rcallCD closureDict (globalPtrRank gptr)
                   (increfObjectC (typeableDict ref) gptr)
           takeMVar mvar
           touchForeignPtr (finalizer ref)

decref :: Ref a -> IO ()
decref ref =
  do let gptr = globalPtr ref
     rexec (globalPtrRank gptr) (decrefObjectC (typeableDict ref) gptr)

decref' :: ClosureDict (Typeable a) -> GlobalPtr (Object a) -> IO ()
decref' dict gptr =
  rexec (globalPtrRank gptr) (decrefObjectC dict gptr)



refFromObject :: Static (Typeable a)
              => GlobalPtr (Object a) -> IO (Ref a)
refFromObject = refFromObjectCD closureDict

refFromObjectCD :: ClosureDict (Typeable a)
                -> GlobalPtr (Object a) -> IO (Ref a)
refFromObjectCD dict gptr =
  do fptr <- mallocForeignPtr
     let ref = Ref dict gptr fptr
     let fin = decref' dict gptr
     FC.addForeignPtrFinalizer fptr fin
     return ref

newRef :: Static (Typeable a)
       => a -> IO (Ref a)
newRef = newRefCD closureDict

newRefCD :: ClosureDict (Typeable a)
         -> a -> IO (Ref a)
newRefCD dict x =
  do gptr <- newObject x
     ref <- refFromObjectCD dict gptr
     return ref

fetchRef :: Static (Serializable a)
         => Ref a -> IO (MVar a)
fetchRef = fetchRefCD closureDict

fetchRefCD :: ClosureDict (Serializable a)
           -> Ref a -> IO (MVar a)
fetchRefCD cd ref =
  do let gptr = globalPtr ref
     rmvar <- rcallCD cd (globalPtrRank gptr) (fetchRef2C cd gptr)
     -- Concurrent execution means that the reference might go away
     -- too early. We need to touch the finalizer after the global
     -- pointer has been accessed. To ensure this, we wait until the
     -- result has been sent back.
     _ <- forkIO do _ <- readMVar rmvar
                    touchForeignPtr (finalizer ref)
     return rmvar

fetchRef2 :: GlobalPtr (Object a) -> IO a
fetchRef2 gptr =
  do mres <- deRefGlobalPtr gptr
     let Just res = mres
     return (object res)

fetchRef2C :: forall a. ClosureDict (Serializable a)
           -> GlobalPtr (Object a) -> Closure (IO a)
fetchRef2C cd gptr =
  withClosureDict cd
  let cd2 = getTypeable cd
      cd3 = closureDictT cd2 :: ClosureDict (Typeable (Object a))
      cd4 = closureDictT cd3 :: ClosureDict (Binary (GlobalPtr (Object a)))
      cd5 = closureDictT cd3 :: ClosureDict (Typeable (GlobalPtr (Object a)))
      cd6 = combineClosureDict cd4 cd5
  in closure (static fetchRef2)
     `cap` cpure cd6 gptr



--------------------------------------------------------------------------------

data SerializedRef a =
  SerializedRef (ClosureDict (Typeable a)) (GlobalPtr (Object a))
  deriving Generic

instance Typeable a => Binary (SerializedRef a) where
  put (SerializedRef dict gptr) = do put dict
                                     put gptr
  get = do dict <- get
           gptr <- get
           return (SerializedRef dict gptr)

{-# NOINLINE serializeRef #-}
serializeRef :: Ref a -> IO (SerializedRef a)
serializeRef ref =
  do let gptr = globalPtr ref
     incref ref
     return (SerializedRef (typeableDict ref) gptr)

{-# NOINLINE deserializeRef #-}
deserializeRef :: SerializedRef a -> IO (Ref a)
deserializeRef (SerializedRef dict gptr) =
  do ref <- refFromObjectCD dict gptr
     -- Note: Don't decrease the reference count here. We are using up
     -- the serialized object, and we are creating a reference, which
     -- combined is refcount-neutral.
     return ref

instance Typeable a => Binary (Ref a) where
  put ref = do let sref = unsafePerformIO (serializeRef ref)
               put sref
  get = do sref <- get
           return (unsafePerformIO (deserializeRef sref))

instance StaticT (Typeable a) (Binary (Ref a)) where
  closureDictT cd = case unclosure cd of Dict -> closure (static dict) `cap` cd
    where dict :: Dict (Typeable b) -> Dict (Binary (Ref b))
          dict Dict = Dict

instance StaticT (Typeable a) (Typeable (Ref a)) where
  closureDictT cd = case unclosure cd of Dict -> closure (static dict) `cap` cd
    where dict :: Dict (Typeable b) -> Dict (Typeable (Ref b))
          dict Dict = Dict



--------------------------------------------------------------------------------

execLocal :: Static (Typeable a)
          => IO a -> IO (Ref a)
execLocal = execLocalCD closureDict

execLocalCD :: ClosureDict (Typeable a)
            -> IO a -> IO (Ref a)
execLocalCD cd act =
  do x <- act
     ref <- newRefCD cd x
     return ref

execLocalC :: ClosureDict (Typeable a)
           -> Closure (IO a) -> Closure (IO (Ref a))
execLocalC cd cl =
  withClosureDict cd $
  closure (static execLocalCD)
  `cap` cduplicate cd
  `cap` cl



local :: Static (Typeable a)
      => IO a -> IO (MVar (Ref a))
local = localCD closureDict

localCD :: ClosureDict (Typeable a)
        -> IO a -> IO (MVar (Ref a))
localCD cd act =
  do mvar <- newEmptyMVar
     _ <- forkIO do ref <- execLocalCD cd act
                    putMVar mvar ref
     return mvar



remote :: Static (Typeable a)
       => MPI.Rank -> Closure (IO a) -> IO (MVar (Ref a))
remote = remoteCD closureDict

remoteCD :: forall a. ClosureDict (Typeable a)
         -> MPI.Rank -> Closure (IO a) -> IO (MVar (Ref a))
remoteCD cd rank act =
  withClosureDict cd
  let cd2 = closureDictT cd :: ClosureDict (Binary (Ref a))
      cd3 = closureDictT cd :: ClosureDict (Typeable (Ref a))
      cd4 = combineClosureDict cd2 cd3
  in rcallCD cd4 rank (execLocalC cd act)
