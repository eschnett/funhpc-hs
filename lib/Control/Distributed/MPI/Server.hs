{-# LANGUAGE TypeApplications #-}

module Control.Distributed.MPI.Server
  ( Message, Send, Recv, Task
  , runServer
  , send
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Foreign.C
import Foreign.Marshal
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.IORef
import System.Exit
import System.IO

import qualified Control.Distributed.MPI as MPI



type Message = B.ByteString
type Send = MPI.Rank -> Message -> IO ()
type Recv = MPI.Rank -> Message -> IO ()
type Task = IO ()



runServer :: Task -> Recv -> IO ()
runServer task recv = bracket
  (do ts <- MPI.initThread MPI.ThreadMultiple
      when (ts < MPI.ThreadMultiple) $
        die ("MPI library only provides " ++ show ts ++ ", while " ++
             show  MPI.ThreadMultiple ++ " is required")
      return ())
  (\_ -> MPI.finalize)
  (\_ -> mainLoop task recv)



comm :: MPI.Comm
comm = MPI.commWorld

tag :: MPI.Tag
tag = MPI.unitTag



mainLoop :: Task -> Recv -> IO ()
mainLoop task recv =
  do rank <- MPI.commRank comm
     size <- MPI.commSize comm
     when (rank == 0) $
       putStrLn $ "Starting MPI server on " ++ show size ++ " processes"

     breq <- newIORef Nothing
     let signalDone = do req <- MPI.ibarrier comm
                         writeIORef breq (Just req)
     let checkDone = do mreq <- readIORef breq
                        case mreq of
                          Nothing -> return False
                          Just req -> MPI.test_ req

     _ <- forkIO $ serve recv

     _ <- forkIO $
       do when (rank == 0) task
          signalDone

     whileM_ (not <$> checkDone) yield

     hFlush stdout
     hFlush stderr
     MPI.barrier comm
     when (rank == 0) $ putStrLn "Done."

     when (rank == 0) $
       do putStrLn "Calling MPI_Abort to terminate the program."
          putStrLn "Please ignore any warnings about MPI_Abort being called."
          putStrLn "All is fine."
     MPI.abort comm 0

     return ()

serve :: Recv -> IO ()
serve recv =
  whileM_ (return True) $
  do mst <- MPI.iprobe MPI.anySource tag comm
     case mst of
       Nothing -> return ()
       Just st -> recvMsg recv st
     yield

recvMsg :: Recv -> MPI.Status -> IO ()
recvMsg recv status =
  do source <- MPI.getSource status
     count <- MPI.getCount status (MPI.datatype @CUChar)
     let len = MPI.fromCount count
     ptr <- mallocBytes len
     MPI.recv_ ptr count source tag comm
     msg <- B.unsafePackMallocCStringLen (ptr, len)
     recv source msg



send :: MPI.Rank -> Message -> IO ()
send dest msg =
  do req <- MPI.isend msg (MPI.toCount (B.length msg)) dest tag comm
     whileM_ (not <$> MPI.test_ req) yield
