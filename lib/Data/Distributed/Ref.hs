{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distributed.Ref
  ( Object
  , Ref
  , touchRef
  , rankRef
  , newRef
  , fetchRef
  , SerializedRef
  , serializeRef
  , deserializeRef
  , local
  , remote
  ) where

-- import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign as F
import Foreign.Concurrent as FC
import GHC.Generics
import System.IO.Unsafe
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.Static
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.Server
import Control.Distributed.MPI.World
import Data.Distributed.Future
import Data.Distributed.GlobalPtr



-- Layout of an object. Objects are stored via 'GlobalPtr', which
-- internally uses 'StablePtr' to allocate and free.
data Object a = Object { count :: !(IORef Word)
                       , object :: a
                       }
  deriving (Eq, Generic)

checkObject :: Object a -> IO ()
checkObject (Object count _) =
  -- do cnt <- readIORef count
  --    assert (cnt > 0) $ return ()
  return ()



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
     checkObject obj
     newcnt <- atomicModifyIORef' (count obj) \cnt -> (cnt + 1, cnt + 1)
     checkObject obj
     return ()

increfObjectC :: Static (Typeable a) => GlobalPtr (Object a) -> Closure (IO ())
increfObjectC gptr = static increfObject `cap` cpure closureDict gptr

decrefObject :: GlobalPtr (Object a) -> IO ()
decrefObject gptr =
  do Just obj <- deRefGlobalPtr gptr
     checkObject obj
     newcnt <- atomicModifyIORef' (count obj) \cnt -> (cnt - 1, cnt - 1)
     assert (newcnt >= 0) $ return ()
     -- Disabling this avoids the segfault
     when (newcnt == 0)
       do -- let !sgptr = show gptr
          -- putStrLn $ "[" ++ show worldRank ++ "] freeGlobalPtr " ++ sgptr
          freeGlobalPtr gptr
          -- putStrLn $ "[" ++ show worldRank ++ "] freeGlobalPtr " ++ sgptr ++ " done"
     -- checkObject obj
     return ()

decrefObjectC :: Static (Typeable a) => GlobalPtr (Object a) -> Closure (IO ())
decrefObjectC gptr = static decrefObject `cap` cpure closureDict gptr



newObject :: a -> IO (GlobalPtr (Object a))
newObject x =
  do cnt <- newIORef 1
     let obj = Object cnt x
     checkObject obj
     newGlobalPtr obj
       
newObjectC :: (Static (Binary a), Static (Typeable a))
           => a -> Closure (IO (GlobalPtr (Object a)))
newObjectC x = static newObject `cap` cpure closureDict x



--------------------------------------------------------------------------------

newtype Ref a = Ref (ForeignPtr (GlobalPtr (Object a)))
  deriving Generic

instance Typeable a => Show (Ref a) where
  show ref@(Ref fptr) =
    "Ref " ++ show fptr ++
    " [" ++ show (unsafeDupablePerformIO (globalPtrRef ref)) ++ "]"

instance Static (Typeable a) => Static (Typeable (Ref a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Ref b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Ref b)))
          dict Dict = Dict



globalPtrRef :: Typeable a => Ref a -> IO (GlobalPtr (Object a))
globalPtrRef (Ref fptr) = withForeignPtr fptr peek

instance Typeable a => Eq (Ref a) where
  ref1 == ref2 = unsafeDupablePerformIO $
                 liftM2 (==) (globalPtrRef ref1) (globalPtrRef ref2)



touchRef :: Typeable a => Ref a -> IO ()
touchRef ref@(Ref fptr) =
  do -- putStrLn $ "[" ++ show worldRank ++ "] touchRef " ++ show ref
     touchForeignPtr fptr



rankRef :: Typeable a => Ref a -> IO MPI.Rank
rankRef ref = do gptr <- globalPtrRef ref
                 let rank = globalPtrRank gptr
                 return rank



incref :: Static (Typeable a) => Ref a -> IO ()
incref ref =
  -- Executing this serially avoids the segfault
  -- DEBUG lexec do gptr <- globalPtrRef ref
  -- DEBUG          fres <- rcall (globalPtrRank gptr) (increfObjectC gptr)
  -- DEBUG          waitFuture fres
  -- DEBUG          touchRef ref
  do gptr <- globalPtrRef ref
     fres <- rcall (globalPtrRank gptr) (increfObjectC gptr)
     waitFuture fres
     touchRef ref

decref :: Static (Typeable a) => Ref a -> IO ()
decref ref = do gptr <- globalPtrRef ref
                rexec (globalPtrRank gptr) (decrefObjectC gptr)

-- TODO maybe the finalizer needs to be rooted?
-- TODO   idea: put all finalizers into a global list, have the finalizer only activate a list element.

decref' :: Static (Typeable a) => GlobalPtr (Object a) -> IO ()
decref' gptr = rexec (globalPtrRank gptr) (decrefObjectC gptr)



refFromObject :: Static (Typeable a) => GlobalPtr (Object a) -> IO (Ref a)
refFromObject gptr =
  do fptr <- mallocForeignPtr
     withForeignPtr fptr \ptr -> poke ptr gptr
     -- Disabling this avoids the segfault
     FC.addForeignPtrFinalizer fptr (decref' gptr)
     return (Ref fptr)
-- refFromObject gptr =
--   do ptr <- malloc
--      poke ptr gptr
--      fptr <- FC.newForeignPtr ptr do decref' gptr
--                                      free ptr
--      return (Ref fptr)



newRef :: Static (Typeable a) => a -> IO (Ref a)
newRef x =
  do gptr <- newObject x
     ref <- refFromObject gptr
     -- This output avoids the segfault
     -- Update: no, it doesn't
     -- putStrLn $ "[" ++ show worldRank ++ "] newRef " ++ show ref
     return ref



-- Note: 'Closure' encodes and decodes serialized values at will, and
-- might re-use serialized values. This instance is not safe. We need
-- to serialize and deserialize manually.

-- instance Static (Typeable a) => Binary (Ref a) where
--   {-# NOINLINE put #-}
--   put ref = do let sref = unsafePerformIO (serializeRef ref)
--                put sref
--   {-# NOINLINE get #-}
--   get = do sref <- get
--            let ref = unsafePerformIO (deserializeRef sref)
--            return ref

-- instance Static (Typeable a) => Static (Binary (Ref a)) where
--   closureDict = static dict `cap` closureDictStatic
--     where dict :: Dict (Static (Typeable b)) -> Dict (Binary (Ref b))
--           dict Dict = Dict
--   closureDictStatic = static dict `cap` closureDictStatic
--     where dict :: Dict (Static (Typeable b))
--                -> Dict (Static (Binary (Ref b)))
--           dict Dict = Dict



fetchRef :: (Static (Binary a), Static (Typeable a)) => Ref a -> IO (Future a)
fetchRef ref =
  do gptr <- globalPtrRef ref
     res <- rcall (globalPtrRank gptr) (fetchRef2C gptr)
     lexec do waitFuture res
              touchRef ref
              -- decref ref
     return res

fetchRef2 :: GlobalPtr (Object a) -> IO a
fetchRef2 gptr =
  do -- putStrLn $ "[" ++ show worldRank ++ "] fetchRef2.a " ++ show gptr
     -- TODO
     -- increfObject gptr
     Just obj <- deRefGlobalPtr gptr
     -- DEBUG checkObject obj
     -- putStrLn $ "[" ++ show worldRank ++ "] fetchRef2.b " ++ show gptr
     -- return (object obj)
     let !res = object obj
     -- TODO
     -- decrefObject gptr
     -- putStrLn $ "[" ++ show worldRank ++ "] fetchRef2.c " ++ show gptr
     return res

fetchRef2C :: Static (Typeable a) => GlobalPtr (Object a) -> Closure (IO a)
fetchRef2C gptr = static fetchRef2 `cap` cpure closureDict gptr



--------------------------------------------------------------------------------

newtype SerializedRef a = SerializedRef (GlobalPtr (Object a))
  deriving (Eq, Ord, Read, Show, Generic)

instance Static (Typeable a) => Binary (SerializedRef a) where
  put (SerializedRef gptr) = put gptr
  get = do gptr <- get
           return (SerializedRef gptr)

instance Static (Typeable a) => Static (Typeable (SerializedRef a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (SerializedRef b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (SerializedRef b)))
          dict Dict = Dict

instance Static (Typeable a) => Static (Binary (SerializedRef a)) where
  closureDict = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b)) -> Dict (Binary (SerializedRef b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary (SerializedRef b)))
          dict Dict = Dict



serializeRef :: Static (Typeable a) => Ref a -> IO (SerializedRef a)
serializeRef ref =
  do gptr <- globalPtrRef ref
     incref ref
     let sref = SerializedRef gptr
     -- putStrLn $ "serializeRef " ++ show sref
     return sref

deserializeRef :: Static (Typeable a) => SerializedRef a -> IO (Ref a)
deserializeRef sref@(SerializedRef gptr) =
  do -- putStrLn $ "deserializeRef " ++ show sref
     -- Note: Don't decrease the reference count here. We are using up
     -- the serialized object, and we are creating a reference, which
     -- combined is refcount-neutral.
     refFromObject gptr



--------------------------------------------------------------------------------

execLocal :: Static (Typeable a) => IO a -> IO (Ref a)
execLocal act =
  do x <- act
     newRef x

execLocalC :: Static (Typeable a) => Closure (IO a) -> Closure (IO (Ref a))
execLocalC cl = static execLocalD `cap` closureDictStatic `cap` cl
  where execLocalD :: Dict (Static (Typeable a)) -> IO a -> IO (Ref a)
        execLocalD Dict = execLocal



local :: Static (Typeable a) => IO a -> IO (Future (Ref a))
local act = forkFuture do execLocal act



remote :: Static (Typeable a)
       => MPI.Rank -> Closure (IO a) -> IO (Future (Ref a))
remote rank act = lcall do fsref <- rcall rank (remote2C act)
                           sref <- readFuture fsref
                           deserializeRef sref

remote2 :: Static (Typeable a) => IO a -> IO (SerializedRef a)
remote2 act = do ref <- execLocal act
                 serializeRef ref

remote2C :: Static (Typeable a)
         => Closure (IO a) -> Closure (IO (SerializedRef a))
remote2C act = static remote2D `cap` closureDictStatic `cap` act
  where remote2D :: Dict (Static (Typeable a)) -> IO a -> IO (SerializedRef a)
        remote2D Dict = remote2
