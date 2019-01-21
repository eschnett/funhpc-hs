{-# LANGUAGE StaticPointers #-}

module Control.Distributed.MPI.Server
  ( runServer
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
import Data.Binary
import Data.IORef
import Data.Maybe
import System.IO
import Type.Reflection

import Control.Distributed.Closure
import Control.Distributed.Closure.Instances()
import Control.Distributed.Closure.StaticT

import Control.Distributed.MPI.World
import qualified Control.Distributed.MPI.Binary as MPI
import Data.Distributed.GlobalPtr



optimizeLocalCalls :: Bool
optimizeLocalCalls = True



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



rcall :: Static (Serializable a)
      => MPI.Rank -> Closure (IO a) -> IO (MVar a)
rcall = rcallCD closureDict

rcallCD :: ClosureDict (Serializable a)
        -> MPI.Rank -> Closure (IO a) -> IO (MVar a)
rcallCD cd rank cl =
  if optimizeLocalCalls && rank == worldRank
  then lcall (unclosure cl)
  else do rmvar <- newEmptyMVar
          rptr <- newGlobalPtr rmvar
          rexec rank (rcall2C cd rptr cl)
          return rmvar

rcall2CD :: ClosureDict (Serializable a)
         -> RVar a -> IO a -> IO ()
rcall2CD cd rptr act =
  do res <- act
     rexec (globalPtrRank rptr) (rcall3C cd rptr res)

rcall2C :: forall a. ClosureDict (Serializable a)
        -> RVar a -> Closure (IO a) -> Closure (IO ())
rcall2C cd rptr cl =
  withClosureDict cd
  let cd2 = getTypeable cd
      cd3 = closureDictT cd2 :: ClosureDict (Typeable (MVar a))
      cd4 = closureDictT cd3 :: ClosureDict (Binary (GlobalPtr (MVar a)))
      cd5 = closureDictT cd3 :: ClosureDict (Typeable (GlobalPtr (MVar a)))
      cd6 = combineClosureDict cd4 cd5
  in closure (static rcall2CD)
  `cap` cduplicate cd
  `cap` cpure cd6 rptr
  `cap` cl

rcall3 :: RVar a -> a -> IO ()
rcall3 rptr res =
  do Just mvar <- deRefGlobalPtr rptr
     putMVar mvar res

rcall3C :: forall a. ClosureDict (Serializable a)
        -> RVar a -> a -> Closure (IO ())
rcall3C cd rptr res =
  withClosureDict cd
  let cd2 = getTypeable cd
      cd3 = closureDictT cd2 :: ClosureDict (Typeable (MVar a))
      cd4 = closureDictT cd3 :: ClosureDict (Binary (GlobalPtr (MVar a)))
      cd5 = closureDictT cd3 :: ClosureDict (Typeable (GlobalPtr (MVar a)))
      cd6 = combineClosureDict cd4 cd5
  in closure (static rcall3)
     `cap` cpure cd6 rptr
     `cap` cpure cd res



rsend :: Static (Serializable a)
      => RVar a -> a -> IO ()
rsend = rsendCD closureDict

rsendCD :: ClosureDict (Serializable a)
        -> RVar a -> a -> IO ()
rsendCD cd gptr val =
  rexec (globalPtrRank gptr) (rsend2C cd gptr val)

rsend2 :: RVar a -> a -> IO ()
rsend2 gptr val =
  do Just mvar <- deRefGlobalPtr gptr
     putMVar mvar val

rsend2C :: forall a. ClosureDict (Serializable a)
        -> RVar a -> a -> Closure (IO ())
rsend2C cd gptr val =
  withClosureDict cd
  let cd2 = getTypeable cd
      cd3 = closureDictT cd2 :: ClosureDict (Typeable (MVar a))
      cd4 = closureDictT cd3 :: ClosureDict (Binary (GlobalPtr (MVar a)))
      cd5 = closureDictT cd3 :: ClosureDict (Typeable (GlobalPtr (MVar a)))
      cd6 = combineClosureDict cd4 cd5
  in closure (static rsend2)
     `cap` cpure cd6 gptr
     `cap` cpure cd val



rfetch :: Static (Serializable a)
       => RVar a -> IO (MVar a)
rfetch = rfetchCD closureDict

rfetchCD :: ClosureDict (Serializable a)
         -> RVar a -> IO (MVar a)
rfetchCD cd gptr =
  rcallCD cd (globalPtrRank gptr) (rfetch2C cd gptr)

rfetch2 :: RVar a -> IO a
rfetch2 gptr =
  do Just mvar <- deRefGlobalPtr gptr
     val <- readMVar mvar
     return val

rfetch2C :: forall a. ClosureDict (Serializable a)
         -> RVar a -> Closure (IO a)
rfetch2C cd gptr =
  withClosureDict cd
  let cd2 = getTypeable cd
      cd3 = closureDictT cd2 :: ClosureDict (Typeable (MVar a))
      cd4 = closureDictT cd3 :: ClosureDict (Binary (GlobalPtr (MVar a)))
      cd5 = closureDictT cd3 :: ClosureDict (Typeable (GlobalPtr (MVar a)))
      cd6 = combineClosureDict cd4 cd5
  in closure (static rfetch2)
     `cap` cpure cd6 gptr
