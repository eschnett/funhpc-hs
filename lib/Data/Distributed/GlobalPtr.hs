{-# LANGUAGE UndecidableInstances #-}

module Data.Distributed.GlobalPtr
  ( GlobalConstraints
  , GlobalPtr(..)
  , newGlobalPtr
  , globalPtrRank
  , deRefGlobalPtr
  , freeGlobalPtr
  , fetchGlobalPtr
  ) where

import Control.Concurrent.MVar
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Foreign
import GHC.Generics

import Data.Binary

import Control.Distributed.MPI.Action
import Data.Distributed.LocalPtr
import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.Server



class (Binary a, LocalConstraints a) => GlobalConstraints a
instance (Binary a, LocalConstraints a) => GlobalConstraints a

data GlobalPtr a = GlobalPtr (LocalPtr a) (LocalPtr (IO B.ByteString))
  deriving (Generic)

instance Eq (GlobalPtr a) where
  GlobalPtr lptr1 _ == GlobalPtr lptr2 _ = lptr1 == lptr2

instance GlobalConstraints a => Binary (GlobalPtr a) where
  put (GlobalPtr lptr sptr) =
    do put lptr
       put sptr
  get =
    do lptr <- get
       sptr <- get
       return (GlobalPtr lptr sptr)

newGlobalPtr :: GlobalConstraints a => a -> IO (GlobalPtr a)
newGlobalPtr x =
  do lptr <- newLocalPtr x
     let (LocalPtr _ ptr) = lptr
     sptr <- newLocalPtr (encDeRef ptr)
     return (GlobalPtr lptr sptr)
       where encDeRef ptr = do val <- deRefStablePtr ptr
                               return (BL.toStrict (encode val))

globalPtrRank :: GlobalPtr a -> MPI.Rank
globalPtrRank (GlobalPtr lptr _) = localPtrRank lptr

deRefGlobalPtr :: GlobalPtr a -> IO (Maybe a)
deRefGlobalPtr (GlobalPtr lptr _) = deRefLocalPtr lptr

freeGlobalPtr :: GlobalPtr a -> IO ()
freeGlobalPtr (GlobalPtr lptr sptr) =
  do freeLocalPtr lptr
     freeLocalPtr sptr

fetchGlobalPtr :: GlobalConstraints a => GlobalPtr a -> IO a
fetchGlobalPtr (GlobalPtr (LocalPtr rank ptr) sptr) =
  if rank == worldRank
  then deRefStablePtr ptr
  else do mvar <- newEmptyMVar
          lPutDec <- newLocalPtr $ putMVar mvar . decode . BL.fromStrict
          rexec rank (FetchGlobalPtr' worldRank lPutDec sptr)
          val <- takeMVar mvar
          freeLocalPtr lPutDec
          return val

data FetchGlobalPtr' = FetchGlobalPtr'
                       MPI.Rank
                       (LocalPtr (B.ByteString -> IO ()))
                       (LocalPtr (IO B.ByteString))
  deriving (Eq, Generic)

instance Binary FetchGlobalPtr'

instance Action FetchGlobalPtr' where
  run (FetchGlobalPtr' rank lPutDec lEncDeRef) =
    do encDeRef <- fromJust <$> deRefLocalPtr lEncDeRef
       buf <- encDeRef
       rexec rank (FetchGlobalPtr'' lPutDec buf)

data FetchGlobalPtr'' = FetchGlobalPtr''
                        (LocalPtr (B.ByteString -> IO ()))
                        B.ByteString
  deriving (Eq, Generic)

instance Binary FetchGlobalPtr''

instance Action FetchGlobalPtr'' where
  run (FetchGlobalPtr'' lPutDec buf) =
    do putDec <- fromJust <$> deRefLocalPtr lPutDec
       putDec buf
