{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Distributed.MPI.Server
  ( runServer
  , remote)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Coerce
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Typeable
import Data.Void
import GHC.Generics
import System.IO
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Control.Distributed.MPI.Packing as MPI



-- | Exception type indicating an error
newtype FunHPCException = FunHPException String
  deriving (Eq, Ord, Read, Show, Typeable)
instance Exception FunHPCException

funHPCAssert :: Bool -> String -> IO ()
funHPCAssert cond msg =
  do when (not cond) $ throw (FunHPException msg)
     return ()



data Ref a = Ref { refRank :: !MPI.Rank, refInt :: !Int }
  deriving (Eq, Ord, Read, Show, Generic)

{-# NOINLINE theNextRef #-}
theNextRef :: IORef (Ref Void)
theNextRef = unsafePerformIO $
  do rank <- MPI.commRank MPI.commWorld
     newIORef (Ref rank 0)

nextRef :: IO (Ref a)
nextRef =
  do ref <- atomicModifyIORef' theNextRef $
       \(Ref rank i) -> (Ref rank (i + 1), Ref rank i)
     return (coerce ref)

data Object where
  Object :: a -> Object
getObject :: Object -> a
getObject (Object obj) = unsafeCoerce obj

{-# NOINLINE theRefTargets #-}
theRefTargets :: IORef (Map.Map (Ref Void) Object)
theRefTargets = unsafePerformIO $ newIORef Map.empty

newRef :: a -> IO (Ref a)
newRef val =
  do ref <- nextRef
     atomicModifyIORef' theRefTargets $
       \rts -> (Map.insert (coerce ref) (Object val) rts, ())
     return ref

{-# NOINLINE resolveRef #-}
resolveRef :: Ref a -> IO a
resolveRef ref =
  do rank <- MPI.commRank MPI.commWorld
     funHPCAssert (rank == refRank ref) "expected rank == refRank"
     obj <- atomicModifyIORef' theRefTargets $
       \rts -> (Map.delete (coerce ref) rts, rts Map.! coerce ref)
     return (getObject obj)



-- Global constants
comm :: MPI.Comm
comm = MPI.commWorld

tag :: MPI.Tag
tag = MPI.unitTag



runServer :: IO () -> IO ()
runServer mainTask =
  MPI.mainMPI $
  do rank <- MPI.commRank comm
     size <- MPI.commSize comm
     when (rank == MPI.rootRank) $
       putStrLn $ "*** Starting MPI server on " ++ show size ++ " processes ***"

     breq <- newIORef Nothing
     let signalDone = do req <- MPI.ibarrier comm
                         writeIORef breq (Just req)
     let checkDone = do mreq <- readIORef breq
                        case mreq of
                          Nothing -> return False
                          Just req -> isJust <$> MPI.test_ req

     _ <- forkIO server

     _ <- forkIO $
       do when (rank == MPI.rootRank) mainTask
          signalDone

     whileM_ (not <$> checkDone) yield

     hFlush stdout
     hFlush stderr
     MPI.barrier comm
     when (rank == 0) $ putStrLn "*** Done. ***"

     when (rank == MPI.rootRank) $
       do putStrLn ""
          putStrLn "Calling MPI_Abort to terminate the program."
          putStrLn "Please ignore any warnings about MPI_Abort being called."
          putStrLn "All is fine."
     MPI.abort comm 0

     return ()



server :: IO ()
server =
  whileM_ (return True) $
  do task <- MPI.recv_ MPI.anySource tag comm
     _ <- forkIO task
     return ()



{-# NOINLINE rexec #-}
rexec :: MPI.Rank -> IO () -> IO ()
rexec dest task = MPI.send task dest tag comm



remote :: MPI.Rank -> (a -> IO b) -> a -> IO (MVar b)
remote dest f x =
  do mvar <- newEmptyMVar
     ref <- newRef mvar
     rexec dest $ remote' ref f x
     return mvar

remote' :: Ref (MVar b) -> (a -> IO b) -> a -> IO ()
remote' !ref f x =
  do r <- f x
     rexec (refRank ref) $ remote'' ref r

remote'' :: Ref (MVar a) -> a -> IO ()
remote'' !ref val =
  do mvar <- resolveRef ref
     putMVar mvar val
