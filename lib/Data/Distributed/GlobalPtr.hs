{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StaticPointers #-}

module Data.Distributed.GlobalPtr
  ( GlobalPtr(..)
  , newGlobalPtr
  , globalPtrRank
  , deRefGlobalPtr
  , freeGlobalPtr
  ) where

import Control.Exception (assert)
import Data.Coerce
import Foreign
import GHC.Generics
import Type.Reflection

import Control.Distributed.Closure
import Control.Distributed.Closure.StaticT
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.World



-- | A pointer to an object that lives on a particular rank
data GlobalPtr a where
  GlobalPtr :: MPI.Rank -> StablePtr a -> GlobalPtr a
  deriving (Eq, Generic)

instance Typeable a => Binary (GlobalPtr a) where
  put (GlobalPtr rank ptr) =
    do put rank
       put @Word (coerce (ptrToWordPtr (castStablePtrToPtr ptr)))
  get =
    do rank <- get
       ptr <- (castPtrToStablePtr . wordPtrToPtr . coerce) <$> get @Word
       return (GlobalPtr rank ptr)

instance StaticT (Typeable a) (Binary (GlobalPtr a)) where
  closureDictT cd = case unclosure cd of Dict -> closure (static dict) `cap` cd
    where dict :: Dict (Typeable b) -> Dict (Binary (GlobalPtr b))
          dict Dict = Dict

instance StaticT (Typeable a) (Typeable (GlobalPtr a)) where
  closureDictT cd = case unclosure cd of Dict -> closure (static dict) `cap` cd
    where dict :: Dict (Typeable b) -> Dict (Typeable (GlobalPtr b))
          dict Dict = Dict

newGlobalPtr :: a -> IO (GlobalPtr a)
newGlobalPtr x =
  do ptr <- newStablePtr x
     return (GlobalPtr worldRank ptr)

globalPtrRank :: GlobalPtr a -> MPI.Rank
globalPtrRank (GlobalPtr rank _) = rank

deRefGlobalPtr :: GlobalPtr a -> IO (Maybe a)
deRefGlobalPtr (GlobalPtr rank ptr) =
  if rank == worldRank
  then Just <$> deRefStablePtr ptr
  else return Nothing

freeGlobalPtr :: GlobalPtr a -> IO ()
freeGlobalPtr (GlobalPtr rank ptr) =
  assert (rank == worldRank) freeStablePtr ptr
