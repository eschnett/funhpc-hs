{-# LANGUAGE StaticPointers #-}

module Control.Distributed.MPI.Server
  ( ClosureDict
  , withClosureDict
  , withClosureDict2
  , runServer
  , lexec
  , lcall
  , rexec
  , rcall
  , rcallCD
  , RVar
  , rsend
  , rsendCD
  , rfetch
  , rfetchCD
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Data.Constraint
import Data.IORef
import Data.Maybe
import System.IO

import Control.Distributed.Closure

import Control.Distributed.MPI.World
import qualified Control.Distributed.MPI.Binary as MPI
import Data.Distributed.GlobalPtr



optimizeLocalCalls :: Bool
optimizeLocalCalls = True



type ClosureDict z = Closure (Dict z)

withClosureDict :: ClosureDict z -> (z => a) -> a
withClosureDict cd x = case unclosure cd of Dict -> x

withClosureDict2 :: ClosureDict z1 -> ClosureDict z2 -> ((z1, z2) => a) -> a
withClosureDict2 cd1 cd2 x =
  case (unclosure cd1, unclosure cd2) of (Dict, Dict) -> x



runServer :: IO () -> IO ()
runServer mainTask =
  MPI.mainMPI
  do when (worldRank == worldRoot)
       do putStrLn ("*** Starting MPI server on " ++ show worldSize ++
                    " processes ***")

     -- TODO: Shouldn't this be atomic? Can we use an 'MVar ()' instead?
     breq <- newIORef Nothing
     let signalDone = do req <- MPI.ibarrier worldComm
                         writeIORef breq (Just req)
     let checkDone = do mreq <- readIORef breq
                        case mreq of
                          Nothing -> return False
                          Just req -> isJust <$> MPI.test_ req

     _ <- forkIO rexecServer

     _ <- forkIO do when (worldRank == worldRoot) mainTask
                    signalDone

     whileM_ (not <$> checkDone) yield

     hFlush stdout
     hFlush stderr
     MPI.barrier worldComm
     when (worldRank == worldRoot) do putStrLn "*** Done. ***"

     when (worldRank == worldRoot)
       do putStrLn ""
          putStrLn "Calling MPI_Abort to terminate the program."
          putStrLn "Please ignore any warnings about MPI_Abort being called."
          putStrLn "All is fine."
     MPI.abort worldComm 0

     return ()



lexec :: IO () -> IO ()
lexec act =
  do _ <- forkIO act
     return ()

lcall :: IO a -> IO (MVar a)
lcall act =
  do mvar <- newEmptyMVar
     _ <- forkIO do x <- act
                    putMVar mvar x
     return mvar



rexecTag :: MPI.Tag
rexecTag = 1

rexec :: MPI.Rank -> Closure (IO ()) -> IO ()
rexec dest cl =
  if optimizeLocalCalls && dest == worldRank
  then lexec (unclosure cl)
  else MPI.send cl dest rexecTag worldComm

rexecServer :: IO ()
rexecServer =
  forever
  do (cl :: Closure (IO ())) <- MPI.recv_ MPI.anySource rexecTag worldComm
     _ <- forkIO do unclosure cl
     return ()



type RVar a = GlobalPtr (MVar a)



rcall :: ( Static (Serializable a)
         , Static (Serializable (RVar a)))
      => MPI.Rank -> Closure (IO a) -> IO (MVar a)
rcall = rcallCD closureDict closureDict

rcallCD :: ClosureDict (Serializable a)
        -> ClosureDict (Serializable (RVar a))
        -> MPI.Rank -> Closure (IO a) -> IO (MVar a)
rcallCD cd1 cd2 rank cl =
  if optimizeLocalCalls && rank == worldRank
  then lcall (unclosure cl)
  else do rmvar <- newEmptyMVar
          rptr <- newGlobalPtr rmvar
          rexec rank (rcall2C cd1 cd2 rptr cl)
          return rmvar

rcall2CD :: ClosureDict (Serializable a)
         -> ClosureDict (Serializable (RVar a))
         -> RVar a -> IO a -> IO ()
rcall2CD cd1 cd2 rptr act =
  do res <- act
     rexec (globalPtrRank rptr) (rcall3C cd1 cd2 rptr res)

rcall2C :: ClosureDict (Serializable a)
        -> ClosureDict (Serializable (RVar a))
        -> RVar a -> Closure (IO a) -> Closure (IO ())
rcall2C cd1 cd2 rptr cl =
  withClosureDict cd1 $
  closure (static rcall2CD)
  `cap` cduplicate cd1
  `cap` cduplicate cd2
  `cap` cpure cd2 rptr
  `cap` cl

rcall3 :: RVar a -> a -> IO ()
rcall3 rptr res =
  do Just mvar <- deRefGlobalPtr rptr
     putMVar mvar res

rcall3C :: ClosureDict (Serializable a)
        -> ClosureDict (Serializable (RVar a))
        -> RVar a -> a -> Closure (IO ())
rcall3C cd1 cd2 rptr res =
  withClosureDict cd1 $
  closure (static rcall3)
  `cap` cpure cd2 rptr
  `cap` cpure cd1 res



rsend :: ( Static (Serializable a)
         , Static (Serializable (RVar a))) 
      => RVar a -> a -> IO ()
rsend = rsendCD closureDict closureDict

rsendCD :: ClosureDict (Serializable a)
        -> ClosureDict (Serializable (RVar a))
        -> RVar a -> a -> IO ()
rsendCD cd1 cd2 gptr val =
  rexec (globalPtrRank gptr) (rsend2C cd1 cd2  gptr val)

rsend2 :: RVar a -> a -> IO ()
rsend2 gptr val =
  do Just mvar <- deRefGlobalPtr gptr
     putMVar mvar val

rsend2C :: ClosureDict (Serializable a)
        -> ClosureDict (Serializable (RVar a))
        -> RVar a -> a -> Closure (IO ())
rsend2C cd1 cd2 gptr val =
  withDict (unclosure cd1) $
  closure (static rsend2)
  `cap` cpure cd2 gptr
  `cap` cpure cd1 val



rfetch :: ( Static (Serializable a)
          , Static (Serializable (RVar a)))
       => RVar a -> IO (MVar a)
rfetch = rfetchCD closureDict closureDict

rfetchCD :: ClosureDict (Serializable a)
         -> ClosureDict (Serializable (RVar a))
         -> RVar a -> IO (MVar a)
rfetchCD cd1 cd2 gptr =
  rcallCD cd1 cd2 (globalPtrRank gptr) (rfetch2C cd1 cd2 gptr)

rfetch2 :: RVar a -> IO a
rfetch2 gptr =
  do Just mvar <- deRefGlobalPtr gptr
     val <- readMVar mvar
     return val

rfetch2C :: ClosureDict (Serializable a)
         -> ClosureDict (Serializable (RVar a))
         -> RVar a -> Closure (IO a)
rfetch2C cd1 cd2 gptr =
  withClosureDict cd1 $
  closure (static rfetch2)
  `cap` cpure cd2 gptr
