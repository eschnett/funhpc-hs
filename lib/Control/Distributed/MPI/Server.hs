module Control.Distributed.MPI.Server
  ( funHPCAssert
  , worldComm
  , worldRank
  , worldRoot
  , worldSize
  , runServer
  , rexec
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.IORef
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Type.Reflection

import Data.Binary

import Control.Distributed.MPI.Action
import qualified Control.Distributed.MPI.Binary as MPI



-- | Exception type indicating an error
newtype FunHPCException = FunHPException String
  deriving (Eq, Ord, Read, Show, Typeable)
instance Exception FunHPCException

funHPCAssert :: Bool -> String -> IO ()
funHPCAssert cond msg = when (not cond) $ throw (FunHPException msg)



-- Global constants
worldComm :: MPI.Comm
worldComm = MPI.commWorld

worldRoot :: MPI.Rank
worldRoot = MPI.rootRank

worldRank :: MPI.Rank
worldRank = unsafePerformIO (MPI.commRank worldComm)

worldSize :: MPI.Rank
worldSize = unsafePerformIO (MPI.commSize worldComm)



serverTag :: MPI.Tag
serverTag = MPI.unitTag



runServer :: IO () -> IO ()
runServer mainTask =
  MPI.mainMPI $
  do when (worldRank == worldRoot) $
       putStrLn ( "*** Starting MPI server on " ++ show worldSize ++
                  " processes ***")

     breq <- newIORef Nothing
     let signalDone = do req <- MPI.ibarrier worldComm
                         writeIORef breq (Just req)
     let checkDone = do mreq <- readIORef breq
                        case mreq of
                          Nothing -> return False
                          Just req -> isJust <$> MPI.test_ req

     _ <- forkIO server

     _ <- forkIO $
       do when (worldRank == worldRoot) mainTask
          signalDone

     whileM_ (not <$> checkDone) yield

     hFlush stdout
     hFlush stderr
     MPI.barrier worldComm
     when (worldRank == worldRoot) $ putStrLn "*** Done. ***"

     when (worldRank == worldRoot) $
       do putStrLn ""
          putStrLn "Calling MPI_Abort to terminate the program."
          putStrLn "Please ignore any warnings about MPI_Abort being called."
          putStrLn "All is fine."
     MPI.abort worldComm 0

     return ()



server :: IO ()
server =
  whileM_ (return True) $
  do (act :: SomeAction) <- MPI.recv_ MPI.anySource serverTag worldComm
     _ <- forkIO (run act)
     return ()



rexec :: (Action a, Binary a, Typeable a) => MPI.Rank -> a -> IO ()
rexec dest act = MPI.send (makeAction act) dest serverTag worldComm
