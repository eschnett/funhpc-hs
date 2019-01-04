module Data.Distributed.LocalPtr
  ( LocalConstraints
  , LocalPtr(..)
  , newLocalPtr
  , localPtrRank
  , deRefLocalPtr
  , freeLocalPtr
  ) where

import Control.Exception (assert)
import Data.Coerce
import Foreign
import Type.Reflection

import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.Server



class Typeable a => LocalConstraints a
instance Typeable a => LocalConstraints a

data LocalPtr a where
  LocalPtr :: LocalConstraints a => MPI.Rank -> StablePtr a -> LocalPtr a

deriving instance Eq (LocalPtr a)

instance LocalConstraints a => Binary (LocalPtr a) where
  put (LocalPtr rank ptr) =
    do put rank
       put @Word (coerce (ptrToWordPtr (castStablePtrToPtr ptr)))
  get =
    do rank <- get
       ptr <- (castPtrToStablePtr . wordPtrToPtr . coerce) <$> get @Word
       return (LocalPtr rank ptr)

newLocalPtr :: LocalConstraints a => a -> IO (LocalPtr a)
newLocalPtr x =
  do ptr <- newStablePtr x
     return (LocalPtr worldRank ptr)

localPtrRank :: LocalPtr a -> MPI.Rank
localPtrRank (LocalPtr rank _) = rank

deRefLocalPtr :: LocalPtr a -> IO (Maybe a)
deRefLocalPtr (LocalPtr rank ptr) =
  if rank == worldRank
  then Just <$> deRefStablePtr ptr
  else return Nothing

freeLocalPtr :: LocalPtr a -> IO ()
freeLocalPtr (LocalPtr rank ptr) =
  assert (rank == worldRank) freeStablePtr ptr
