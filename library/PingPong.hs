{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module PingPong (main) where

import Control.Concurrent (forkIO, yield)
import Control.Exception
import Control.Monad (when)
import Control.Monad.Loops (whileM_)
import qualified Control.Parallel.MPI.Fast as Fast
import Control.Parallel.MPI.Simple
import Data.Array.Storable
import Data.Foldable
import Data.Maybe



main :: IO ()
main = bracket_ (do ts <- initThread Multiple
                    when (ts < Multiple) $
                         throwIO $ PatternMatchFail "initThread < Multiple"
                )
                finalize
                (do rank <- commRank commWorld
                    putStrLn "a.0"
                    forkIO worker
                    putStrLn "a.1"
                    when (rank == 0) manager
                    putStrLn "a.2"
                )



manager :: IO ()
manager =
    do putStrLn "PingPong"
       size <- commSize commWorld
       for_ [toRank (0 :: Int) .. toRank (size-1)] $ \p ->
           do arr <- newArray_ @StorableArray @Int (0 :: Int, -1)
              putStrLn "m.0"
              Fast.send commWorld p unitTag arr
              putStrLn "m.1"
              -- (msg, _status) <- recv commWorld p unitTag
              -- putStrLn ("[" ++ show p ++ "]: " ++ msg)
              req <- Fast.irecv commWorld p unitTag arr
              putStrLn "m.2"
              whileM_ (isNothing <$> Fast.test req) yield
              putStrLn "m.3"
              Fast.wait req
              putStrLn "m.4"
              putStrLn ("[" ++ show p ++ "]: " ++ "Hello, World!")

worker :: IO ()
worker =
    do rank <- commRank commWorld
       putStrLn $ "Worker " ++ show rank
       -- receive a message...
       arr <- newArray_ @StorableArray @Int (0 :: Int, -1)
       putStrLn "w.0"
       req <- Fast.irecv commWorld 0 unitTag arr
       putStrLn "w.1"
       whileM_ (isNothing <$> Fast.test req) yield
       putStrLn "w.2"
       Fast.wait req
       putStrLn "w.3"
       -- ...and send one back
       -- send commWorld 0 unitTag ("Hello, World from process" ++ show rank)
       Fast.send commWorld 0 unitTag arr
       putStrLn "w.4"
