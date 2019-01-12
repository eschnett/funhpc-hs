{-# LANGUAGE StaticPointers #-}

module Control.Distributed.MPI.Server
  ( runServer
  , rexec
  , rsend
  , rfetch
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
     -- _ <- forkIO rsendServer
     -- _ <- forkIO rfetchServer
     -- _ <- forkIO rexec'Server

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

rsend :: forall a.
         ( Static (Binary a), Static (Binary (GMVar a))
         , Static (Typeable a), Static (Typeable (GMVar a)))
      => GMVar a -> a -> IO ()
rsend gptr val =
  do let fun :: Closure ((GMVar a, a) -> IO ())
         fun = closure (static putGptr)
         args :: Closure (GMVar a, a)
         args = cpure closureDict (gptr, val)
         cl :: Closure (IO ())
         cl = fun `cap` args
     rexec (globalPtrRank gptr) cl

putGptr :: (GMVar a, a) -> IO ()
putGptr (gptr, val) =
  do Just mvar <- deRefGlobalPtr gptr
     putMVar mvar val



rfetch :: forall a.
          ( Static (Binary a), Static (Binary (GMVar a))
          , Static (Typeable a), Static (Typeable (GMVar a)))
       => GMVar a -> IO a
rfetch gptr =
  do mvar <- newEmptyMVar
     rptr <- newGlobalPtr mvar
     let fun :: Closure ( Dict ( Static (Binary a)
                               , Static (Binary (GMVar a))
                               , Static (Typeable a)
                               , Static (Typeable (GMVar a))) ->
                          (GMVar a, GMVar a) -> IO ())
         fun = closure (static \Dict -> readGptr)
     (args :: Closure (GMVar a, GMVar a)) <-
       return (cpure closureDict (rptr, gptr))
     (cl :: Closure (IO ())) <- return (fun `cap` closureDict `cap` args)
     rexec (globalPtrRank gptr) cl
     res <- takeMVar mvar
     freeGlobalPtr rptr
     return res

readGptr :: ( Static (Binary a), Static (Binary (GMVar a))
            , Static (Typeable a), Static (Typeable (GMVar a)))
         => (GMVar a, GMVar a) -> IO ()
readGptr (rptr, gptr) =
  do Just mvar <- deRefGlobalPtr gptr
     val <- readMVar mvar
     rsend rptr val



----------------------------------------------------------------------



-- | Static 3-tuples
instance ( Static c1, Static c2, Static c3
         , Typeable c1, Typeable c2, Typeable c3
         , (c1, c2, c3)) =>
         Static (c1, c2, c3) where
  closureDict =
    static tupleDict `cap` closureDict `cap` closureDict `cap` closureDict
    where tupleDict :: Dict d1 -> Dict d2 -> Dict d3 -> Dict (d1, d2, d3)
          tupleDict Dict Dict Dict = Dict

-- | Static 4-tuples
instance ( Static c1, Static c2, Static c3, Static c4
         , Typeable c1, Typeable c2, Typeable c3, Typeable c4
         , (c1, c2, c3, c4)) =>
         Static (c1, c2, c3, c4) where
  closureDict =
    static tupleDict `cap`
    closureDict `cap` closureDict `cap` closureDict `cap` closureDict
    where tupleDict :: Dict d1 -> Dict d2 -> Dict d3 -> Dict d4
                    -> Dict (d1, d2, d3, d4)
          tupleDict Dict Dict Dict Dict = Dict



-- | Static Binary tuples
instance ( Static (Binary a), Static (Binary b)
         , Typeable a, Typeable b
         , Binary (a, b)) =>
         Static (Binary (a, b)) where
  closureDict = static pairDict `cap` closureDict `cap` closureDict
    where pairDict :: Dict (Binary x) -> Dict (Binary y) -> Dict (Binary (x, y))
          pairDict Dict Dict = Dict

-- | Static Typeable tuples
instance ( Static (Typeable a), Static (Typeable b)
         , Typeable a, Typeable b) =>
         Static (Typeable (a, b)) where
  closureDict = static pairDict `cap` closureDict `cap` closureDict
    where pairDict ::
            Dict (Typeable x) -> Dict (Typeable y) -> Dict (Typeable (x, y))
          pairDict Dict Dict = Dict



-- | Static Statics
instance Static c => Static (Static c) where
  closureDict = closureDict
