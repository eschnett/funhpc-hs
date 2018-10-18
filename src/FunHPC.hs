{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import qualified Control.Distributed.MPI as MPI
import Control.Distributed.MPI.Server

main :: IO ()
main = runServer task action

task :: IO ()
task =
  do putStrLn "Hello, World!"
     rank <- MPI.commRank MPI.commWorld
     size <- MPI.commSize MPI.commWorld
     forM_ [0..size-1] $ \r -> send r "greetings"

action :: MPI.Rank -> Message -> IO ()
action src msg =
  do rank <- MPI.commRank MPI.commWorld
     putStrLn $ "[" ++ show rank ++ " <- " ++ show src ++ "] " ++
                "received message: " ++ show msg
     when (rank /= 0) $
       send 0 "thank you"
