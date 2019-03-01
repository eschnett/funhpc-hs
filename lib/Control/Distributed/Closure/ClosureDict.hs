module Control.Distributed.Closure.ClosureDict
  ( ClosureDict
  , withClosureDict
  , withClosureDict2
  , withClosureDict3
  , withClosureDict4
  , callWithClosureDict
  ) where

import Control.Distributed.Closure



type ClosureDict z = Closure (Dict z)

withClosureDict :: ClosureDict z -> (z => a) -> a
withClosureDict cd x = case unclosure cd of Dict -> x

withClosureDict2 :: ClosureDict z1 -> ClosureDict z2 -> ((z1, z2) => a) -> a
withClosureDict2 cd1 cd2 x =
  case (unclosure cd1, unclosure cd2) of (Dict, Dict) -> x

withClosureDict3 :: ClosureDict z1
                 -> ClosureDict z2
                 -> ClosureDict z3
                 -> ((z1, z2, z3) => a)
                 -> a
withClosureDict3 cd1 cd2 cd3 x =
  case (unclosure cd1, unclosure cd2, unclosure cd3) of (Dict, Dict, Dict) -> x

withClosureDict4 :: ClosureDict z1
                 -> ClosureDict z2
                 -> ClosureDict z3
                 -> ClosureDict z4
                 -> ((z1, z2, z3, z4) => a)
                 -> a
withClosureDict4 cd1 cd2 cd3 cd4 x =
  case (unclosure cd1, unclosure cd2, unclosure cd3, unclosure cd4) of
    (Dict, Dict, Dict, Dict) -> x

callWithClosureDict :: (z => a) -> ClosureDict z -> a
callWithClosureDict f = \cd -> case unclosure cd of Dict -> f
