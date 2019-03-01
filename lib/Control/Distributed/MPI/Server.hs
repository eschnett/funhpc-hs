{-# LANGUAGE StaticPointers #-}

module Control.Distributed.MPI.Server
  ( runServer
  , lexec
  , lcall
  , rexec
  , RVar
  , rcall
  , rsend
  , rfetch
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Data.Binary
import Data.Maybe
import System.IO
import Type.Reflection

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.ClosureDict
import Control.Distributed.Closure.Static

import Control.Distributed.MPI.World
import qualified Control.Distributed.MPI.Binary as MPI
import Data.Distributed.Future
import Data.Distributed.GlobalPtr



optimizeLocalCalls :: Bool
optimizeLocalCalls = False      --TODO True



runServer :: IO () -> IO ()
runServer mainTask =
  MPI.mainMPI
  do when (worldRank == worldRoot)
       do putStrLn ("*** Starting MPI server on " ++ show worldSize ++
                    " processes ***")
          hFlush stdout

     breq <- newEmptyFuture
     let signalDone = do req <- MPI.ibarrier worldComm
                         putFuture breq req
     let checkDone = do mreq <- tryReadFuture breq
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
                                      hFlush stdout

     when (worldRank == worldRoot)
       do putStrLn ""
          putStrLn "Calling MPI_Abort to terminate the program."
          putStrLn "Please ignore any warnings about MPI_Abort being called."
          putStrLn "All is fine."
          hFlush stdout
     MPI.abort worldComm 0

     return ()



forkIO_ :: IO () -> IO ()
forkIO_ s = do _ <- forkIO s
               return ()



type RVar a = GlobalPtr (Future a)



lexec :: IO () -> IO ()
lexec = forkIO_

lcall :: IO a -> IO (Future a)
lcall = forkFuture



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
     lexec (unclosure cl)



rcall :: ( Static (Binary a)
         , Static (Typeable a))
      => MPI.Rank -> Closure (IO a) -> IO (Future a)
rcall rank cl =
  if optimizeLocalCalls && rank == worldRank
  then lcall (unclosure cl)
  else do rmvar <- newEmptyFuture
          rptr <- newGlobalPtr rmvar
          rexec rank (rcall2C rptr cl)
          return rmvar

rcall2 :: ( Static (Binary a)
          , Static (Typeable a))
       => RVar a -> IO a -> IO ()
rcall2 rptr act =
  do res <- act
     rexec (globalPtrRank rptr) (rcall3C rptr res)

rcall2C :: ( Static (Binary a)
           , Static (Typeable a))
        => RVar a -> Closure (IO a) -> Closure (IO ())
rcall2C rptr cl =
  static rcall2D
  `cap` closureDictStatic
  `cap` closureDictStatic
  `cap` cpure closureDict rptr
  `cap` cl
  where rcall2D :: Dict (Static (Binary a))
                -> Dict (Static (Typeable a))
                -> RVar a -> IO a -> IO ()
        rcall2D Dict Dict = rcall2

rcall3 :: RVar a -> a -> IO ()
rcall3 rptr res =
  do Just mvar <- deRefGlobalPtr rptr
     putFuture mvar res

rcall3C :: ( Static (Binary a)
           , Static (Typeable a))
        => RVar a -> a -> Closure (IO ())
rcall3C rptr res =
  static rcall3 `cap` cpure closureDict rptr `cap` cpure closureDict res



rsend :: ( Static (Binary a)
         , Static (Typeable a))
      => RVar a -> a -> IO ()
rsend gptr val =
  rexec (globalPtrRank gptr) (rsend2C gptr val)

rsend2 :: RVar a -> a -> IO ()
rsend2 gptr val =
  do Just mvar <- deRefGlobalPtr gptr
     putFuture mvar val

rsend2C :: ( Static (Binary a)
           , Static (Typeable a))
        => RVar a -> a -> Closure (IO ())
rsend2C gptr val =
  static rsend2
  `cap` cpure closureDict gptr
  `cap` cpure closureDict val



rfetch :: ( Static (Binary a)
          , Static (Typeable a))
       => RVar a -> IO (Future a)
rfetch gptr =
  rcall (globalPtrRank gptr) (rfetch2C gptr)

rfetch2 :: RVar a -> IO a
rfetch2 gptr =
  do Just mvar <- deRefGlobalPtr gptr
     val <- readFuture mvar
     return val

rfetch2C :: Static (Typeable a)
         => RVar a -> Closure (IO a)
rfetch2C gptr =
  static rfetch2
  `cap` cpure closureDict gptr
