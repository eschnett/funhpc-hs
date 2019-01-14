{-# LANGUAGE StaticPointers #-}

module Control.Distributed.MPI.Server
  ( runServer
  , rexec
  , rsend
  , rfetch
  , local
  , remote
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



runServer :: IO () -> IO ()
runServer mainTask =
  MPI.mainMPI
  do when (worldRank == worldRoot)
       do putStrLn ("*** Starting MPI server on " ++ show worldSize ++
                    " processes ***")

     breq <- newIORef Nothing
     let signalDone = do req <- MPI.ibarrier worldComm
                         writeIORef breq (Just req)
     let checkDone = do mreq <- readIORef breq
                        case mreq of
                          Nothing -> return False
                          Just req -> isJust <$> MPI.test_ req

     _ <- forkIO rexecServer

     _ <- forkIO
       do when (worldRank == worldRoot) mainTask
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



rexecTag :: MPI.Tag
rexecTag = 1

rexec :: MPI.Rank -> Closure (IO ()) -> IO ()
rexec dest cl = MPI.send cl dest rexecTag worldComm

rexecServer :: IO ()
rexecServer =
  forever
  do (cl :: Closure (IO ())) <- MPI.recv_ MPI.anySource rexecTag worldComm
     _ <- forkIO do unclosure cl
     return ()



type GMVar a = GlobalPtr (MVar a)



rsend :: (Static (Serializable (GMVar a)), Static (Serializable a))
      => GMVar a -> a -> IO ()
rsend = rsend' closureDict closureDict

rsend' :: Closure (Dict (Serializable (GMVar a)))
       -> Closure (Dict (Serializable a))
       -> GMVar a
       -> a
       -> IO ()
rsend' cd1 cd2 gptr val =
  case unclosure cd2 of
    Dict ->
      let fun = closure (static putGptr)
          arg1 = cpure cd1 gptr
          arg2 = cpure cd2 val
      in rexec (globalPtrRank gptr) $ fun `cap` arg1 `cap` arg2

putGptr :: GMVar a -> a -> IO ()
putGptr gptr val =
  do Just mvar <- deRefGlobalPtr gptr
     putMVar mvar val



rfetch :: (Static (Serializable (GMVar a)), Static (Serializable a))
       => GMVar a -> IO a
rfetch = rfetch' closureDict closureDict

rfetch' :: Closure (Dict (Serializable (GMVar a)))
        -> Closure (Dict (Serializable a))
        -> GMVar a
        -> IO a
rfetch' cd1 cd2 gptr =
  case unclosure cd2 of
    Dict ->
      do mvar <- newEmptyMVar
         rptr <- newGlobalPtr mvar
         let fun = closure (static readGptr)
             arg1 = cduplicate cd1
             arg2 = cduplicate cd2
         arg3 <- return (cpure cd1 rptr)
         let arg4 = cpure cd1 gptr
         cl <- return (fun `cap` arg1 `cap` arg2 `cap` arg3 `cap` arg4)
         rexec (globalPtrRank gptr) cl
         res <- takeMVar mvar
         freeGlobalPtr rptr
         return res

readGptr :: Closure (Dict (Serializable (GMVar a)))
         -> Closure (Dict (Serializable a))
         -> GMVar a
         -> GMVar a
         -> IO ()
readGptr cd1 cd2 rptr gptr =
  do Just mvar <- deRefGlobalPtr gptr
     val <- readMVar mvar
     rsend' cd1 cd2 rptr val



local :: (a -> IO b) -> a -> IO (MVar b)
local fun arg =
  do mvar <- newEmptyMVar
     _ <- forkIO do res <- fun arg
                    putMVar mvar res
     return mvar



-- TODO: keep the value on the remote system, return a 'GlobalPtr' ('Ref') to it
remote :: (Static (Serializable (GMVar a)), Static (Serializable a))
       => MPI.Rank -> Closure (IO a) -> IO (MVar a)
remote = remote' closureDict closureDict

remote' :: Closure (Dict (Serializable (GMVar a)))
        -> Closure (Dict (Serializable a))
        -> MPI.Rank
        -> Closure (IO a)
        -> IO (MVar a)
remote' cd1 cd2 rank cl =
  case unclosure cd2 of
    Dict ->
      do mvar <- newEmptyMVar
         rptr <- newGlobalPtr mvar
         let fun = closure (static execSend)
             arg1 = cduplicate cd1
             arg2 = cduplicate cd2
         arg3 <- return (cpure cd1 rptr)
         cl' <- return (fun `cap` arg1 `cap` arg2 `cap` arg3 `cap` cl)
         rexec rank cl'
         return mvar

execSend :: Closure (Dict (Serializable (GMVar a)))
         -> Closure (Dict (Serializable a))
         -> GMVar a
         -> IO a
         -> IO ()
execSend cd1 cd2 gptr cl =
  do res <- cl
     rsend' cd1 cd2 gptr res
