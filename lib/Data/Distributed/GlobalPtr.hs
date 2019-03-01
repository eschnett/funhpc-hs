{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distributed.GlobalPtr
  ( GlobalPtr(..)
  , newGlobalPtr
  , globalPtrRank
  , deRefGlobalPtr
  , freeGlobalPtr
  ) where

import Control.Exception
import Data.Coerce
import Foreign
import Foreign.C.Types
import GHC.Generics
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.Static
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.World



-- | A pointer to an object that lives on a particular rank
data GlobalPtr a = GlobalPtr !MPI.Rank !(StablePtr a)
  deriving (Eq, Generic)

instance Typeable a => Binary (GlobalPtr a) where
  put (GlobalPtr rank ptr) =
    do put rank
       put @Word (coerce (ptrToWordPtr (castStablePtrToPtr ptr)))
  get =
    do rank <- get
       ptr <- (castPtrToStablePtr . wordPtrToPtr . coerce) <$> get @Word
       return (GlobalPtr rank ptr)

instance Typeable a => Storable (GlobalPtr a) where
  sizeOf _ =
    let offset = max
                 (sizeOf (undefined::CInt))
                 (alignment (undefined::StablePtr a))
    in offset + sizeOf (undefined::StablePtr a)
  alignment _ =
    max (alignment (undefined::CInt)) (alignment (undefined::StablePtr a))
  poke ptr (GlobalPtr rank sptr) =
    do let irank :: CInt = coerce rank
       let offset = max (sizeOf (undefined::CInt)) (alignment sptr)
       pokeByteOff (castPtr ptr) 0 irank
       pokeByteOff (castPtr ptr) offset sptr
  peek ptr =
    do let offset =
             max (sizeOf (undefined::CInt)) (alignment (undefined::StablePtr a))
       irank :: CInt <- peekByteOff (castPtr ptr) 0
       sptr <- peekByteOff (castPtr ptr) offset
       let rank = coerce irank
       return (GlobalPtr rank sptr)

instance Static (Typeable a) => Static (Typeable (GlobalPtr a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (GlobalPtr b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (GlobalPtr b)))
          dict Dict = Dict

instance Static (Typeable a) => Static (Binary (GlobalPtr a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Binary (GlobalPtr b))
          dict Dict = Dict
  closureDictStatic = static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary (GlobalPtr b)))
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
