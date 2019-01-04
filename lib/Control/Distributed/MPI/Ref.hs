{-# LANGUAGE UndecidableInstances #-}

module Control.Distributed.MPI.Ref
  (
  ) where

import Prelude hiding (rem)

import Control.Concurrent.MVar
import Control.Exception (assert)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Coerce
import Data.Dynamic
import Data.IORef
import Data.Kind
import Data.Map.Strict
import Data.Maybe
import Data.Proxy
import Data.Word
import Foreign
import Foreign.C.Types
import Foreign.Concurrent as FC
import Foreign.Ptr
import GHC.Generics
import System.IO.Unsafe
import Type.Reflection

import Data.Binary

import Data.CDynamic
import Data.Distributed.GlobalPtr
import Control.Distributed.MPI.Action
import Control.Distributed.MPI.Server
import qualified Control.Distributed.MPI.Binary as MPI



data GlobalRefOwner a = GlobalRefOwner (GlobalPtr a) (IORef Word64)
  deriving (Eq, Generic)

newGlobalRefOwner :: GlobalPtr a -> IO (GlobalRefOwner a)
newGlobalRefOwner ptr =
  do cnt <- newIORef 1
     return (GlobalRefOwner ptr cnt)

incGlobalRefOwner :: GlobalRefOwner a -> IO ()
incGlobalRefOwner (GlobalRefOwner _ cnt) =
  atomicModifyIORef' cnt $ \c -> (c + 1, ())

decGlobalRefOwner :: GlobalRefOwner a -> IO ()
decGlobalRefOwner (GlobalRefOwner ptr cnt) =
  do c' <- atomicModifyIORef' cnt $ \c -> (c - 1, c - 1)
     if c' == 0
       then freeGlobalPtr ptr
       else return ()



-- data GlobalRef a = GlobalRef (GlobalPtr a) (GlobalRefOwner a) (ForeignPtr ())
--   deriving (Eq, Generic)
-- 
-- instance Binary (GlobalRef a) where
--   put = _
--   get = _
-- 
-- newGlobalRef :: a -> IO (GlobalRef a)
-- newGlobalRef x =
--   do ptr <- newGlobalPtr x
--      own <- newGlobalRefOwner ptr
--      fin <- mallocForeignPtr
--      FC.addForeignPtrFinalizer fin (decGlobalRefOwner own)
--      return (GlobalRef ptr own fin)
-- 
-- globalRefRank :: GlobalRef a -> MPI.Rank
-- globalRefRank (GlobalRef ptr _ _) = globalPtrRank ptr
-- 
-- deRefGlobalRef :: GlobalRef a -> IO (Maybe a)
-- deRefGlobalRef (GlobalRef ptr _ _) = deRefGlobalPtr ptr
-- 
-- fetchGlobalRef :: GlobalRef a -> IO (GlobalRef a)
-- fetchGlobalRef = _



-- --------------------------------------------------------------------------------
-- 
-- type Key = Word64
-- type Count = Word64
-- data Value = Value { object :: CDynamic
--                    , refcount :: Count
--                    , finalize :: IO ()
--                    }
--   deriving Generic
-- 
-- objects :: IORef (Map Key Value)
-- objects = unsafePerformIO (newIORef empty)
-- 
-- nextKey :: IORef Key
-- nextKey = unsafePerformIO (newIORef 0)
-- 
-- makeKey :: IO Key
-- makeKey = atomicModifyIORef' nextKey $ \k -> (k + 1, k)
-- 
-- 
-- 
-- -- | A local reference, to be used either with exlicit freeing or
-- -- reference counting
-- newtype Loc a = Loc Key
--   deriving (Eq, Ord, Read, Show, Generic)
-- 
-- instance Binary (Loc a)
-- 
-- newLocWithFinalizer :: (Binary a, Typeable a) => IO () -> a -> IO (Loc a)
-- newLocWithFinalizer fin x =
--   do key <- makeKey
--      let val = Value (toCDyn x) 1 fin
--      atomicModifyIORef' objects $ \objs -> (insert key val objs, ())
--      return (Loc key)
-- 
-- newLoc :: (Binary a, Typeable a) => a -> IO (Loc a)
-- newLoc = newLocWithFinalizer (return ())
-- 
-- freeLoc :: Loc a -> IO ()
-- freeLoc (Loc key) = atomicModifyIORef' objects $ \objs -> (delete key objs, ())
-- 
-- getDynLoc :: Loc a -> IO CDynamic
-- getDynLoc (Loc key) =
--   do objs <- readIORef objects
--      return (object (objs ! key))
-- 
-- getLoc :: (Binary a, Typeable a) => Loc a -> IO (Maybe a)
-- getLoc (Loc key) =
--   do objs <- readIORef objects
--      return (fromCDynamic (object (objs ! key)))
-- 
-- incRefCount :: Loc a -> IO ()
-- incRefCount (Loc key) =
--   atomicModifyIORef' objects $ \objs -> (adjust inc key objs, ())
--   where inc val = val { refcount = refcount val + 1 }
-- 
-- decRefCount :: Loc a -> IO ()
-- decRefCount (Loc key) =
--   do fin <- atomicModifyIORef' objects $
--             \objs -> let (retval, newobjs) = alterF dec key objs
--                      in (newobjs, retval)
--      fin
--   where dec mval = let val = fromJust mval
--                        rc = refcount val - 1
--                    in if rc == 0
--                       then (finalize val, Nothing)
--                       else (return (), Just (val { refcount = rc }))
-- 
-- 
-- 
-- -- | A reference to a remote object, without reference counting
-- data Rem a = Rem !MPI.Rank !(Loc a)
--   deriving (Eq, Ord, Read, Show, Generic)
-- 
-- instance Binary (Rem a)
-- 
-- newRemFromLoc :: Loc a -> Rem a
-- newRemFromLoc loc = Rem worldRank loc
-- 
-- newRem :: (Binary a, Typeable a) => a -> IO (Rem a)
-- newRem x = newRemFromLoc <$> newLoc x
-- 
-- freeRem :: Rem a -> IO ()
-- freeRem rem = freeLoc (fromJust (getLocFromRem rem))
-- 
-- getLocFromRem :: Rem a -> Maybe (Loc a)
-- getLocFromRem rem@(Rem rank loc) = if remIsLocal rem then Just loc else Nothing
-- 
-- remRank :: Rem a -> MPI.Rank
-- remRank (Rem rank _) = rank
-- 
-- remIsLocal :: Rem a -> Bool
-- remIsLocal rem = remRank rem == worldRank
-- 
-- getRem :: (Binary a, Typeable a) => Rem a -> IO (Maybe a)
-- getRem rem =
--   do let mloc = getLocFromRem rem
--      case mloc of Nothing -> return Nothing
--                   Just loc -> getLoc loc
-- 
-- newtype BinStablePtr a =
--   BinStablePtr { getBinStablePtr :: StablePtr a }
--   deriving (Eq, Generic)
-- 
-- instance Binary (BinStablePtr a) where
--   put = put @Word . coerce . ptrToWordPtr . castStablePtrToPtr . getBinStablePtr
--   get = BinStablePtr . castPtrToStablePtr . wordPtrToPtr . coerce <$> get @Word
-- 
-- fetchRem :: Typeable a => Rem a -> IO a
-- fetchRem (Rem rank loc) =
--   do mres <- newEmptyMVar
--      let putmres :: CDynamic -> IO ()
--          putmres res = putMVar mres (fromJust (fromCDynamic res))
--      sputmres <- BinStablePtr <$> newStablePtr putmres
--      rexec rank (FetchRem' worldRank sputmres (coerce loc))
--      res <- takeMVar mres
--      freeStablePtr (getBinStablePtr sputmres)
--      return res
-- 
-- data FetchRem' = FetchRem' MPI.Rank (BinStablePtr (CDynamic -> IO ())) (Loc ())
--   deriving (Eq, Generic)
-- 
-- instance Action FetchRem' where
--   run (FetchRem' rank sputmres loc) =
--     do res <- getDynLoc loc
--        rexec rank (FetchRem'' sputmres res)
-- 
-- instance Binary FetchRem'
-- 
-- data FetchRem'' = FetchRem'' (BinStablePtr (CDynamic -> IO ())) CDynamic
--   deriving (Generic)
-- 
-- instance Action FetchRem'' where
--   run (FetchRem'' sputmres res) =
--     do putmres <- deRefStablePtr (getBinStablePtr sputmres)
--        putmres res
-- 
-- instance Binary FetchRem''
-- 
-- _ = registerAction @FetchRem'
-- _ = registerAction @FetchRem''
-- 
-- 
-- 
-- -- -- | A reference to a remote object, with reference counting
-- -- data Ref a = Ref { remote :: !(Rem a)
-- --                  , refcounter :: !(Loc ())
-- --                  , finalizer :: !(Ptr ())
-- --                  }
-- --   deriving (Eq, Ord, Read, Show, Generic)
-- -- 
-- -- newRefFromRem :: Rem a -> IO (Ref a)
-- -- newRefFromRem rem =
-- --   do rc <- newLocWithFinalizer (decRef (getLocFromRem rem))
-- --      fp <- mallocForeignPtr
-- --      addForeignPtrFinalizer fp (decRef rc)
-- --      return (Ref rem rc fp)
-- -- 
-- -- newRef :: Typeable a => a -> IO (Ref a)
-- -- newRef x = newRefFromRem <$> newRem x
-- -- 
-- -- GOOD^
-- -- BADv
-- --
-- -- refRank :: Ref a -> MPI.Rank
-- -- refRank (Ref rem _) = remRank rem
-- -- 
-- -- refIsLocal :: Ref a -> Bool
-- -- refIsLocal (Ref rem _) = remIsLocal rem
-- -- 
-- -- getRef :: Typeable a => Ref a -> IO (Maybe a)
-- -- getRef (Ref rem _) = getRem rem
-- -- 
-- -- 
-- -- 
-- -- data SerializedRef a = SerializedRef !(Rem a)
-- --   deriving (Eq, Ord, Read, Show, Generic)
-- -- 
-- -- instance Store (SerializedRef a)
-- -- 
-- -- serializeRef :: Ref a -> IO (SerializedRef a)
-- -- serializeRef (Ref rem _) =
-- --   assert (remIsLocal rem) $
-- --   do incRefCount (fromJust (getLocFromRem rem))
-- --      return (SerializedRef rem)
-- -- 
-- -- deserializeRef :: SerializedRef a -> IO (Ref a)
-- -- deserializeRef (SerializedRef rem) =
-- --   if remIsLocal rem
-- --   then do decRefCount (fromJust (getLocFromRem rem))
-- --           return (Ref rem Nothing)
-- --   else do pxy <- newLoc ()
-- --           return (Ref rem (Just pxy))
-- -- 
-- -- -- fetchRef :: Ref a -> IO a
-- -- -- fetchRef = _
-- 
-- 
-- 
-- -- -- | A reference to a remote object, with reference counting
-- -- data Ref a = Ref { objectRef :: !(Ref a)
-- --                  , refCount :: !RefCount
-- --                  , finalizerHook :: !Ptr ()
-- --                  }
-- --   deriving (Eq, Ord, Show, Generic)
-- -- 
-- -- -- | Create a 'Ref' from a local object
-- -- newRef :: Typeable a => a -> IO (Ref a)
-- -- newRef val =
-- --   do ref <- newRem val
-- --      cnt <- makeRefCount $ freeRem ref
-- --      ptr <- mallocForeignPtr
-- --      FC.addForeignPtrFinalizer ptr (decRefCount loc)
-- --      return (Ref ptr loc)
-- -- 
-- -- -- | Get an object from an already-local 'Ref'
-- -- getRef :: Typeable a => Ref a -> IO (Maybe a)
-- -- getRef (Ref ref _) = getRem ref
-- -- 
-- -- -- | A serialized 'Ref' that can be sent. Each 'SerializedRef'
-- -- -- must be unserialized exactly once for reference counting to work.
-- -- data SerializedRef a = SerializedRef { obj :: !(Ref a)
-- --                                            , toFree :: !(Ref a)
-- --                                            }
-- --   deriving (Eq, Ord, Read, Show, Generic)
-- -- 
-- -- serializeRef (Ref ref _) =
-- --   do incRef key
-- --      ptr <- mallocForeignPtr
-- --      FC.addForeignPtrFinalizer ptr (decRef key)
-- -- 
-- -- ref' <- newRef ()
-- --      
-- -- 
-- -- 
-- -- 
-- -- -- Ref:
-- -- --    rank
-- -- --    key (of object)
-- -- --    fin (finalizer)
-- -- 
-- -- -- Remote:
-- -- --    
-- -- 
-- -- --    rank (where object lives)
-- -- --    key (of object)
-- -- --    its finalizer (to trigger freeing object)
-- -- 
-- -- --    fake object
-- -- --    its finalizer (to trigger freeing fake object)
-- -- 
-- -- 
-- -- 
-- -- data Remote a = LocalRef { refKey :: !Key
-- --                          , refFinalizer :: !Finalizer
-- --                          }
-- --               | RemoteRef { refObjectRank :: !MPI.Rank
-- --                           , refObjectKey :: !Key
-- --                           , refSourceRank :: !MPI.Rank
-- --                           , refSourceKey :: !MPI.Rank
-- --                           , refProxyKey :: !Key
-- --                           , refProxyFinalizer :: !Finalizer
-- --                           }
-- --   deriving (Eq, Ord, Show, Generic)
-- -- 
-- -- -- | A "wrapped" 'Remote' that can be serialized
-- -- data WrappedRemote a = WrappedRemote !MPI.Rank !Key !MPI.Rank !Key
-- --   deriving (Eq, Ord,Read, Show, Generic)
-- -- 
-- -- wrapRemote :: Remote a -> IO (WrappedRemote a)
-- -- wrapRemote (LocalRef key _) =
-- --   do rank <- MPI.commRank MPI.commWorld
-- --      incRef key
-- --      return $ WrappedRemote rank key rank key
-- -- wrapRemote (RemoteRef rank key pkey _) =
-- --   do incRef pkey
-- --      return $ WrappedRemote rank key
-- -- 
-- -- unwrapRemote :: WrappedRemote a -> IO (Remote a)
-- -- unwrapRemote wr@(WrappedRemote rank key srank skey) =
-- --   do (pkey, pfin) <- newRef wr
-- --      FC.addForeignPtrFinalizer pfin $ rexec srank (decRef skey)
-- --      return $ RemoteRef rank key pkey pfin
-- -- 
-- -- 
-- -- 
-- -- 
-- -- 
-- -- -- | Fetch an object from a 'Remote'
-- -- fetchRemote :: Typeable a => Remote a -> IO (MVar a)
-- -- fetchRemote remote =
-- --   do rank <- MPI.commRank MPI.commWorld
-- --      if rank == remoteRank remote
-- --        then getRemote remote
-- --        else fetchRemote' remote
-- -- 
-- -- fetchRemote' :: Typeable a => Remote a -> IO (MVar a)
-- -- fetchRemote' remote =
-- --   do result <- newEmptyMVar
-- --      result' <- newRemote result
-- -- 
-- -- 
-- -- -- -- | Copy a 'Remote' to make it local
-- -- -- copyRemote :: Remote -> Remote
