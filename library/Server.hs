{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module Server (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Parallel.MPI.Fast as Fast
-- import Control.Parallel.MPI.Simple
import Data.Array.Storable
import Data.Foldable
import Data.IORef
import Data.Maybe



main :: IO ()
main = bracket_ (do ts <- initThread Multiple
                    when (ts < Multiple) $
                         throwIO $ PatternMatchFail "initThread < Multiple"
                )
                finalize
                (do rank <- commRank commWorld
                    sendqueue <- newMVar []
                    termsig <- newEmptyMVar
                    let sendto = exec sendqueue
                    forkIO $ do when (rank == 0) (action sendto)
                                putMVar termsig ()
                    server sendqueue termsig
                    return ()
                )



type Msg = ()

server :: MVar [(Rank, Msg)] -> MVar () -> IO ()
server sendqueue termsig =
    do rank <- commRank commWorld
       putStrLn $ "[Server " ++ show rank ++ " starting]"
       sendreqs <- prepareSends
       recvreq <- prepareRecv
       termreq <- prepareTerminate
       whileM_ (not <$> terminateDone termreq)
                 (do handleSendQueue sendqueue sendreqs
                     handleSends sendreqs
                     handleRecv recvreq
                     checkTerminate termsig termreq
                 )
       finalizeSendQueue sendqueue
       finalizeSends sendreqs
       finalizeRecv recvreq
       putStrLn $ "[Server " ++ show rank ++ " done]"
       return ()

exec :: MVar [(Rank, Msg)] -> Rank -> Msg -> IO ()
exec sendqueue rank msg =
    do sq <- takeMVar sendqueue
       let newsq = (rank, msg) : sq
       putMVar sendqueue newsq



type Buf = StorableArray Int Int

handleSendQueue :: MVar [(Rank, Msg)] -> IORef [(Buf, Request)] -> IO ()
handleSendQueue sendqueue sendreqs =
    do sq <- takeMVar sendqueue
       putMVar sendqueue []
       newreqs <- forM sq $ \(rank, msg) ->
                    do buf <- newArray_ @StorableArray @Int (0 :: Int, -1)
                       req <- isend commWorld rank unitTag buf
                       putStrLn "isend"
                       return (buf, req)
       oldreqs <- readIORef sendreqs
       writeIORef sendreqs (newreqs ++ oldreqs)

finalizeSendQueue :: MVar [(Rank, Msg)] -> IO ()
finalizeSendQueue sendqueue =
    do _ <- takeMVar sendqueue
       putMVar sendqueue []



prepareSends :: IO (IORef [(Buf, Request)])
prepareSends = newIORef []

handleSends :: IORef [(Buf, Request)] -> IO ()
handleSends sendreqs =
    do reqs <- readIORef sendreqs
       -- TODO: use testSome
       newreqs <- filterM (\(buf, req) -> isNothing <$> test req) reqs
       writeIORef sendreqs newreqs

finalizeSends :: IORef [(Buf, Request)] -> IO ()
finalizeSends sendreqs =
    do reqs <- readIORef sendreqs
       -- TODO: use cancelAll
       mapM_ (\(buf, req) -> cancel req) reqs
       writeIORef sendreqs []



prepareRecv :: IO (IORef (Buf, Request))
prepareRecv =
    do buf <- newArray_ @StorableArray @Int (0 :: Int, -1)
       req <- irecv commWorld 0 unitTag buf
       newIORef (buf, req)

handleRecv :: IORef (Buf, Request) -> IO ()
handleRecv recvreq =
    do (buf, req) <- readIORef recvreq
       mst <- test req
       when (isJust mst) $
            do -- mst <- iprobe commWorld 0 unitTag
               -- arr <- newArray_ @StorableArray @Int (0 :: Int, -1)
               -- send commWorld 0 unitTag arr
               rank <- commRank commWorld
               putStrLn $ "recv " ++ show rank
               newreq <- irecv commWorld 0 unitTag buf
               writeIORef recvreq (buf, newreq)

finalizeRecv :: IORef (Buf, Request) -> IO ()
finalizeRecv recvreq =
    do (buf, req) <- readIORef recvreq
       cancel req



prepareTerminate :: IO (IORef (Maybe Request))
prepareTerminate = newIORef Nothing

checkTerminate :: MVar () -> IORef (Maybe Request) -> IO ()
checkTerminate termsig termreq =
    do term <- tryTakeMVar termsig
       when (isJust term) $
            do mreq <- readIORef termreq
               when (isNothing mreq) $
                    do req <- ibarrier commWorld
                       writeIORef termreq (Just req)

terminateDone :: IORef (Maybe Request) -> IO Bool
terminateDone termreq =
    do mreq <- readIORef termreq
       case mreq of
         Nothing -> return False
         Just req -> do mst <- test req
                        case mst of
                          Nothing -> return False
                          Just _ -> return True



action :: (Rank -> Msg -> IO ()) -> IO ()
action sendto =
    do putStrLn "Hello, World!"
       size <- commSize commWorld
       for_ [0 .. size-1] $ \p -> sendto (toRank p) ()
       putStrLn "waiting..."
       threadDelay 1000000
       putStrLn "Done."
