module Control.Distributed.MPI.World
  ( funHPCAssert
  , worldComm
  , worldRank
  , worldRoot
  , worldSize
  ) where

import Control.Exception
import Control.Monad
import System.IO.Unsafe
import Type.Reflection

import qualified Control.Distributed.MPI.Binary as MPI



-- | Exception type indicating an error
newtype FunHPCException = FunHPException String
  deriving (Eq, Ord, Read, Show, Typeable)
instance Exception FunHPCException

-- TODO: get rid of this
funHPCAssert :: Bool -> String -> IO ()
funHPCAssert cond msg = when (not cond) do throw (FunHPException msg)

-- TODO: export this
assertIO :: Bool -> IO ()
assertIO cond = assert cond return ()



-- Global constants
worldComm :: MPI.Comm
worldComm = MPI.commWorld

worldRoot :: MPI.Rank
worldRoot = MPI.rootRank

worldRank :: MPI.Rank
worldRank = unsafePerformIO (MPI.commRank worldComm)

worldSize :: MPI.Rank
worldSize = unsafePerformIO (MPI.commSize worldComm)
