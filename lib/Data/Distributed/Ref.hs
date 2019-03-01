{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distributed.Ref
 ( Object
 , Ref
 , newRef
 , rankRef
 , fetchRef
 , local
 , remote
 ) where

-- import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign
import Foreign.Concurrent as FC
import GHC.Generics
import System.IO.Unsafe
import System.Mem
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.ClosureDict
import Control.Distributed.Closure.Static
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.Server
import Data.Distributed.Future
import Data.Distributed.GlobalPtr

import Debug.Trace



-- Layout of an object. Objects are stored via 'GlobalPtr', which
-- internally uses 'StablePtr' to allocate and free.
data Object a = Object { count :: !(IORef Word)
                       , object :: !(IORef a)
                       }

instance Static (Typeable a) => Static (Typeable (Object a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Object b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Object b)))
          dict Dict = Dict



increfObject :: GlobalPtr (Object a) -> IO ()
increfObject gptr =
  do Just obj <- deRefGlobalPtr gptr
     newcnt <- atomicModifyIORef' (count obj) \cnt -> (cnt + 1, cnt + 1)
     -- putStrLn $ "incref newcnt=" ++ show newcnt
     return ()

increfObjectC :: Static (Typeable a) => GlobalPtr (Object a) -> Closure (IO ())
increfObjectC gptr = static increfObject `cap` cpure closureDict gptr

decrefObject :: GlobalPtr (Object a) -> IO ()
decrefObject gptr =
  do Just obj <- deRefGlobalPtr gptr
     newcnt <- atomicModifyIORef' (count obj) \cnt -> (cnt - 1, cnt - 1)
     -- putStrLn $ "decref newcnt=" ++ show newcnt
     when (newcnt == 0) do freeGlobalPtr gptr
                           performGC
     return ()

decrefObjectC :: Static (Typeable a) => GlobalPtr (Object a) -> Closure (IO ())
decrefObjectC gptr = static decrefObject `cap` cpure closureDict gptr

newObject :: a -> IO (GlobalPtr (Object a))
newObject x =
  do cnt <- newIORef 1
     x' <- newIORef x
     obj <- return (Object cnt x')
     -- putStrLn "newobj cnt=1"
     gptr <- newGlobalPtr obj
     return gptr
  
newObjectC :: ( Static (Binary a)
              , Static (Typeable a))
           => a -> Closure (IO (GlobalPtr (Object a)))
newObjectC x = static newObject `cap` cpure closureDict x



--------------------------------------------------------------------------------

newtype BoxedGlobalPtr a = BoxedGlobalPtr (ForeignPtr (GlobalPtr a))

boxGlobalPtr :: Typeable a => GlobalPtr a -> IO (BoxedGlobalPtr a)
boxGlobalPtr gptr = do bptr <- mallocForeignPtr
                       withForeignPtr bptr \ptr -> poke ptr gptr
                       return (BoxedGlobalPtr bptr)

unboxGlobalPtr :: Typeable a => BoxedGlobalPtr a -> IO (GlobalPtr a)
unboxGlobalPtr (BoxedGlobalPtr bptr) = withForeignPtr bptr peek



data Ref a = Ref { typeableDict :: !(ClosureDict (Typeable a))
                 , boxedPtr :: !(BoxedGlobalPtr (Object a))
                 }
  deriving Generic



--TODO instance Typeable a => Eq (Ref a) where
--TODO   ref1 == ref2 = unsafeDupablePerformIO do gptr1 <- globalPtr ref1
--TODO                                            gptr2 <- globalPtr ref2
--TODO                                            let !eq = gptr1 == gptr2
--TODO                                            return eq

instance Static (Typeable a) => Static (Typeable (Ref a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Ref b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Ref b)))
          dict Dict = Dict

instance Static (Typeable a) => Static (Binary (Ref a)) where
  closureDict = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b)) -> Dict (Binary (Ref b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b)) -> Dict (Static (Binary (Ref b)))
          dict Dict = Dict



globalPtr :: Typeable a => Ref a -> IO (GlobalPtr (Object a))
globalPtr ref = do let bptr = boxedPtr ref
                   gptr <- unboxGlobalPtr bptr
                   return gptr



incref :: Static (Typeable a) => Ref a -> IO ()
incref ref =
  lexec do gptr <- globalPtr ref
           fvar <- rcall (globalPtrRank gptr) (increfObjectC gptr)
           sptr <- newStablePtr ref
           waitFuture fvar
           -- disabling this avoids the segfault
           freeStablePtr sptr

decref :: Static (Typeable a) => Ref a -> IO ()
decref ref = do gptr <- globalPtr ref
                rexec (globalPtrRank gptr) (decrefObjectC gptr)

decref' :: Static (Typeable a)
        => GlobalPtr (Object a) -> IO ()
decref' gptr = rexec (globalPtrRank gptr) (decrefObjectC gptr)



refFromObject :: Static (Typeable a) => GlobalPtr (Object a) -> IO (Ref a)
refFromObject gptr =
  do bptr <- boxGlobalPtr gptr
     let BoxedGlobalPtr fptr = bptr
     FC.addForeignPtrFinalizer fptr (decref' gptr)
     return (Ref closureDict bptr)



newRef :: Static (Typeable a) => a -> IO (Ref a)
newRef x =
  do gptr <- newObject x
     ref <- refFromObject gptr
     return ref



rankRef :: Typeable a => Ref a -> IO MPI.Rank
rankRef ref = do gptr <- globalPtr ref
                 return (globalPtrRank gptr)



-- fetchRef :: (Static (Binary a), Static (Typeable a), Static (NFData a))
--          => Ref a -> IO (Future a)
-- fetchRef ref =
--   do traceIO "fetchRef.1"
--      let gptr = globalPtr ref
--      traceIO "fetchRef.2"
--      rfvar <- rcall (globalPtrRank gptr) (fetchRef2C gptr)
--      -- Concurrent execution means that the reference might go away
--      -- too early. We need to touch the finalizer after the global
--      -- pointer has been accessed. To ensure this, we wait until the
--      -- result has been sent back.
--      traceIO "fetchRef.3"
--      _ <- forkIO do traceIO "fetchRef.7"
--                     x <- readFuture rfvar
--                     evaluate $ force x
--                     traceIO "fetchRef.8"
--                     touchForeignPtr (finalizer ref)
--                     traceIO "fetchRef.9"
--      traceIO "fetchRef.4"
--      -- _ <- readFuture rfvar
--      -- traceIO "fetchRef.5"
--      -- touchForeignPtr (finalizer ref)
--      -- traceIO "fetchRef.6"
--      return rfvar
-- 
-- fetchRef2 :: GlobalPtr (Object a) -> IO a
-- fetchRef2 gptr =
--   do traceIO "fetchRef2.1"
--      mres <- deRefGlobalPtr gptr
--      traceIO "fetchRef2.2"
--      let Just res = mres
--      traceIO "fetchRef2.3"
--      return (object res)
-- 
-- fetchRef2C :: (Static (Binary a), Static (Typeable a))
--            => GlobalPtr (Object a) -> Closure (IO a)
-- fetchRef2C gptr =
--   static fetchRef2 `cap` cpure closureDict gptr

fetchRef :: (Static (Binary a), Static (Typeable a), Static (NFData a))
         => Ref a -> IO (Future a)
fetchRef ref =
  do rank <- rankRef ref
     rfvar <- rcall rank (fetchRef2C ref)
     lexec do sptr <- newStablePtr ref
              -- waitFuture rfvar
              x <- readFuture rfvar
              evaluate $ force x
              -- disabling this DOES NOT avoid the segfault
              freeStablePtr sptr
              return ()
     return rfvar

fetchRef2 :: (Typeable a, NFData a) => Ref a -> IO a
fetchRef2 ref =
  do sptr <- newStablePtr ref
     !gptr <- globalPtr ref
     Just !obj <- deRefGlobalPtr gptr
     -- TODO
     !rx <- evaluate (object obj)
     -- let rx = object obj
     !x <- readIORef rx
     !y <- evaluate $ force x
     -- disabling this avoids the segfault
     freeStablePtr sptr
     return y

fetchRef2C :: (Static (Typeable a), Static (NFData a))
           => Ref a -> Closure (IO a)
fetchRef2C ref =
  static fetchRef2D
  `cap` closureDict
  `cap` closureDict
  `cap` cpure closureDict ref
  where fetchRef2D :: Dict (Typeable a) -> Dict (NFData a) -> Ref a -> IO a
        fetchRef2D Dict Dict = fetchRef2



--------------------------------------------------------------------------------

data SerializedRef a =
  SerializedRef !(ClosureDict (Typeable a)) !(GlobalPtr (Object a))
  deriving Generic

instance Typeable a => Binary (SerializedRef a) where
  put (SerializedRef dict gptr) = do put dict
                                     put gptr
  get = do dict <- get
           gptr <- get
           let !sref = SerializedRef dict gptr
           return sref

{-# NOINLINE serializeRef #-}
serializeRef :: Static (Typeable a) => Ref a -> IO (SerializedRef a)
serializeRef ref =
  do gptr <- globalPtr ref
     incref ref
     let !sref = SerializedRef (typeableDict ref) gptr
     return sref

{-# NOINLINE deserializeRef #-}
deserializeRef :: Static (Typeable a) => SerializedRef a -> IO (Ref a)
deserializeRef (SerializedRef _ gptr) =
  do ref <- refFromObject gptr
     -- Note: Don't decrease the reference count here. We are using up
     -- the serialized object, and we are creating a reference, which
     -- combined is refcount-neutral.
     return ref

instance Static (Typeable a) => Binary (Ref a) where
  put ref = do let !sref = unsafePerformIO (serializeRef ref)
               put sref
  get = do sref <- get
           let !res = unsafePerformIO (deserializeRef sref)
           return res



--------------------------------------------------------------------------------

execLocal :: Static (Typeable a) => IO a -> IO (Ref a)
execLocal act =
  do x <- act
     ref <- newRef x
     return ref

execLocalC :: Static (Typeable a) => Closure (IO a) -> Closure (IO (Ref a))
execLocalC cl =
  static execLocalD
  `cap` closureDictStatic
  `cap` cl
  where execLocalD :: Dict (Static (Typeable a)) -> IO a -> IO (Ref a)
        execLocalD Dict = execLocal



local :: Static (Typeable a) => IO a -> IO (Future (Ref a))
local act = forkFuture do execLocal act



remote :: Static (Typeable a)
       => MPI.Rank -> Closure (IO a) -> IO (Future (Ref a))
remote rank act = rcall rank (execLocalC act)
