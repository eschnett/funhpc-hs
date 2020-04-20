{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distributed.GlobalPtr
  ( GlobalPtr(..)
  , newGlobalPtr
  , globalPtrRank
  , deRefGlobalPtr
  , freeGlobalPtr
  ) where

import Control.Concurrent
import Control.Exception
import Data.Coerce
import Data.IORef
import Data.Kind
import Foreign
import Foreign.C.Types
import GHC.Generics
import System.IO.Unsafe
import System.Mem
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.Static
import Data.Binary

import qualified Control.Distributed.MPI.Binary as MPI
import Control.Distributed.MPI.World



-- | A pointer to an object that lives on a particular rank
data GlobalPtr (a :: Type) = GlobalPtr !MPI.Rank !Word
  deriving (Eq, Ord, Read, Show, Generic)

data Repr a = Repr !(MVar a) !(IORef Bool)

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
       Just sptr -> do Repr mvar valid <- deRefStablePtr sptr
                       val <- readIORef valid
                       assert val $ return ()
                       emp <- isEmptyMVar mvar
                       assert (not emp) $ return ()
     return (rank, word)



instance Typeable a => Binary (GlobalPtr a) where
  --DEBUG put gptr = let (rank, word) = checkPtr gptr in do put rank
  --DEBUG                                                   put word
  --DEBUG get = do rank <- get
  --DEBUG          word <- get
  --DEBUG          let gptr = GlobalPtr rank word
  --DEBUG          let (rank', word') = checkPtr gptr
  --DEBUG          return (GlobalPtr rank' word')
  put (GlobalPtr rank word) = do put rank
                                 put word
  get = do rank <- get
           word <- get
           return (GlobalPtr rank word)

-- | Round up 'i' to the next nearest multiple of 'j'. We expect 'i >=
-- 0' and 'j > 0'. With 'r = alignUp i j' we ensure 'r >= i && r < i +
-- j'.
{-# INLINE alignUp #-}
alignUp :: Integral a => a -> a -> a
alignUp i j = ((i + j - 1) `div` j) * j

instance Typeable a => Storable (GlobalPtr a) where
  sizeOf _ = let szi = sizeOf (undefined::CInt)
                 szw = sizeOf (undefined::Word)
                 alw = alignment (undefined::Word)
                 offset = alignUp szi alw
             in offset + szw
  alignment _ = let ali = alignment (undefined::CInt)
                    alw = alignment (undefined::Word)
                in max ali alw
  --DEBUG poke ptr gptr = let (rank, word) = checkPtr gptr in
  poke ptr (GlobalPtr rank word) =
    do let irank :: CInt = coerce rank
       let szi = sizeOf (undefined::CInt)
           alw = alignment (undefined::Word)
           offset = alignUp szi alw
       poke (castPtr ptr) irank
       pokeByteOff (castPtr ptr) offset word
  peek ptr =
    do let szi = sizeOf (undefined::CInt)
           alw = alignment (undefined::Word)
           offset = alignUp szi alw
       irank :: CInt <- peek (castPtr ptr)
       word <- peekByteOff (castPtr ptr) offset
       let rank = coerce irank
       --DEBUG let gptr = GlobalPtr rank word
       --DEBUG let (rank', word') = checkPtr gptr
       --DEBUG return (GlobalPtr rank' word')
       return (GlobalPtr rank word)

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
     x' <- newMVar x
     sptr <- newStablePtr (Repr x' valid)
     return (fromStablePtr sptr)

globalPtrRank :: GlobalPtr a -> MPI.Rank
globalPtrRank (GlobalPtr rank _) = rank

deRefGlobalPtr :: GlobalPtr a -> IO (Maybe a)
deRefGlobalPtr gptr = case toStablePtr gptr of
                        Nothing -> return Nothing
                        Just sptr -> do Repr x valid <- deRefStablePtr sptr
                                        val <- readIORef valid
                                        assert val $ return ()
                                        emp <- isEmptyMVar x
                                        assert (not emp) $ return ()
                                        x' <- readMVar x
                                        return (Just x')

freeGlobalPtr :: GlobalPtr a -> IO ()
freeGlobalPtr gptr = case toStablePtr gptr of
                       Nothing -> error "Tried to free remote GlobalPtr"
                       Just sptr -> do repr@(Repr x valid) <- deRefStablePtr sptr
                                       val <- readIORef valid
                                       assert val $ return ()
                                       atomicWriteIORef valid False
                                       emp <- isEmptyMVar x
                                       assert (not emp) $ return ()
                                       _ <- takeMVar x
                                       -- _ <- forkIO $ putStrLn $ "freeGlobalPtr" ++ show gptr
                                       freeStablePtr sptr
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- performGC
                                       -- yield
                                       -- mvar <- newEmptyMVar
                                       -- ioref <- newIORef False
                                       -- _ <- newStablePtr (Repr mvar ioref)
                                       -- return ()
