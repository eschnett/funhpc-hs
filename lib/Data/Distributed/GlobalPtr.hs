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
import Data.IORef
import Data.Kind
import Foreign
import Foreign.C.Types
import GHC.Generics
import System.IO.Unsafe
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.Static
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
-- TODO? import qualified Control.Distributed.MPI.Storable()
import Control.Distributed.MPI.World



-- | A pointer to an object that lives on a particular rank
data GlobalPtr (a :: Type) = GlobalPtr MPI.Rank Word
  deriving (Eq, Ord, Read, Show, Generic)

data Repr a = Repr a (IORef Bool)

fromStablePtr :: StablePtr (Repr a) -> GlobalPtr a
fromStablePtr sptr =
  GlobalPtr worldRank (coerce (ptrToWordPtr (castStablePtrToPtr sptr)))

toStablePtr :: GlobalPtr a -> Maybe (StablePtr (Repr a))
toStablePtr (GlobalPtr rank word) =
  if rank == worldRank
  then Just (castPtrToStablePtr (wordPtrToPtr (coerce word)))
  else Nothing

{-# NOINLINE checkPtr #-}
checkPtr :: GlobalPtr a -> (MPI.Rank, Word)
checkPtr gptr@(GlobalPtr rank word) =
  unsafePerformIO $
  do let msptr = toStablePtr gptr
     case msptr of
       Nothing -> return ()
       Just sptr -> do Repr _ valid <- deRefStablePtr sptr
                       val <- readIORef valid
                       assert val $ return ()
     return (rank, word)



instance Typeable a => Binary (GlobalPtr a) where
  --TODO put (GlobalPtr rank word) = do put rank
  --TODO                                put word
  --TODO get = do rank <- get
  --TODO          word <- get
  --TODO          return (GlobalPtr rank word)
  put gptr = let (rank, word) = checkPtr gptr in do put rank
                                                    put word
  get = do rank <- get
           word <- get
           let gptr = GlobalPtr rank word
           let (rank', word') = checkPtr gptr
           return (GlobalPtr rank' word')

instance Typeable a => Storable (GlobalPtr a) where
  sizeOf _ = sizeOf (undefined::CInt) + sizeOf (undefined::Word)
  alignment _ = 1
  --TODO poke ptr (GlobalPtr rank word) =
  poke ptr gptr = let (rank, word) = checkPtr gptr in
    do let irank :: CInt = coerce rank
       let offset = sizeOf irank
       pokeByteOff (castPtr ptr) 0 irank
       pokeByteOff (castPtr ptr) offset word
  peek ptr =
    do let offset = sizeOf (undefined::CInt)
       irank :: CInt <- peekByteOff (castPtr ptr) 0
       word <- peekByteOff (castPtr ptr) offset
       let rank = coerce irank
       --TODO return (GlobalPtr rank word)
       let gptr = GlobalPtr rank word
       let (rank', word') = checkPtr gptr
       return (GlobalPtr rank' word')

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
  do valid <- newIORef True
     sptr <- newStablePtr (Repr x valid)
     return (fromStablePtr sptr)

globalPtrRank :: GlobalPtr a -> MPI.Rank
globalPtrRank (GlobalPtr rank _) = rank

deRefGlobalPtr :: GlobalPtr a -> IO (Maybe a)
deRefGlobalPtr gptr = case toStablePtr gptr of
                        Nothing -> return Nothing
                        Just sptr -> do Repr x valid <- deRefStablePtr sptr
                                        val <- readIORef valid
                                        assert val $ return ()
                                        return (Just x)

freeGlobalPtr :: GlobalPtr a -> IO ()
freeGlobalPtr gptr = case toStablePtr gptr of
                       Nothing -> error "Tried to free remote GlobalPtr"
                       Just sptr -> do Repr _ valid <- deRefStablePtr sptr
                                       val <- readIORef valid
                                       assert val $ return ()
                                       atomicWriteIORef valid False
                                       freeStablePtr sptr
