{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops (whileJust_, whileM_)
import Control.Parallel.MPI.Fast as Fast
import qualified Control.Parallel.MPI.Simple as Simple
import qualified Data.ByteString as B
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Serialize.Text ()
import qualified Data.Text as T
import Data.Text.Encoding



main :: IO ()
main = bracket_ (do ts <- initThread Multiple
                    when (ts < Multiple) $
                         throwIO $ PatternMatchFail "initThread < Multiple"
                )
                finalize $
                do rank <- commRank commWorld
                   sendqueue <- newMVar []
                   termsig <- newEmptyMVar
                   let sendto = exec sendqueue
                   forkIO $ do when (rank == 0) (manager sendto)
                               putMVar termsig ()
                   server sendqueue termsig
                   return ()



type Msg = T.Text

server :: MVar [(Rank, Msg)] -> MVar () -> IO ()
server sendqueue termsig =
    do rank <- commRank commWorld
       putStrLn $ "[" ++ show rank ++ "] Server starting"
       sendreqs <- prepareSends
       termreq <- prepareTerminate
       whileM_ (not <$> terminateDone termreq) $
         do handleSendQueue sendqueue sendreqs
            handleSends sendreqs
            handleRecv
            checkTerminate termsig termreq
       finalizeSendQueue sendqueue
       finalizeSends sendreqs
       putStrLn $ "[" ++ show rank ++ "] Server done"
       return ()

exec :: MVar [(Rank, Msg)] -> Rank -> Msg -> IO ()
exec sendqueue rank msg =
    do sq <- takeMVar sendqueue
       let newsq = (rank, msg) : sq
       putMVar sendqueue newsq

exec' :: Msg -> IO ()
exec' msg =
  do rank <- commRank commWorld
     putStrLn $ "[" ++ show rank ++ "]: " ++ show msg



type Buf = B.ByteString

handleSendQueue :: MVar [(Rank, Msg)] -> IORef [(Buf, Request)] -> IO ()
handleSendQueue sendqueue sendreqs =
    do sq <- takeMVar sendqueue
       putMVar sendqueue []
       newreqs <- forM sq $ \(rank, msg) ->
                    do let buf = encodeUtf8 msg
                       req <- Simple.isendBS commWorld rank unitTag buf
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
       newreqs <- filterM (\(_, req) -> isNothing <$> test req) reqs
       writeIORef sendreqs newreqs

finalizeSends :: IORef [(Buf, Request)] -> IO ()
finalizeSends sendreqs =
    do reqs <- readIORef sendreqs
       -- TODO: don't cancel
       mapM_ (\(_, req) -> cancel req) reqs
       writeIORef sendreqs []



handleRecv :: IO ()
handleRecv =
  whileJust_ (iprobe commWorld 0 unitTag) $ \_ ->
             do (buf, _) <- Simple.recvBS commWorld 0 unitTag
                let msg = decodeUtf8 buf
                forkIO $ exec' msg



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



manager :: (Rank -> Msg -> IO ()) -> IO ()
manager sendto =
    do putStrLn "Hello, World!"
       size <- commSize commWorld
       for_ [0 .. size-1] $ \p -> sendto (toRank p) "Hello, World!"
       putStrLn "waiting..."
       threadDelay 1000000
       putStrLn "Done."
