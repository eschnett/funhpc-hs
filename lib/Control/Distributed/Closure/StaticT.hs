{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Distributed.Closure.StaticT
  ( ClosureDict
  , withClosureDict
  , withClosureDict2
  , getTypeable
  , combineClosureDict
  , StaticT(..)
  ) where

-- import Data.Binary
import Data.Constraint
import Type.Reflection

import Control.Distributed.Closure



type ClosureDict z = Closure (Dict z)

withClosureDict :: ClosureDict z -> (z => a) -> a
withClosureDict cd x = case unclosure cd of Dict -> x

withClosureDict2 :: ClosureDict z1 -> ClosureDict z2 -> ((z1, z2) => a) -> a
withClosureDict2 cd1 cd2 x =
  case (unclosure cd1, unclosure cd2) of (Dict, Dict) -> x



getTypeable :: ClosureDict (Serializable a) -> ClosureDict (Typeable a)
getTypeable cd = withClosureDict cd $ closure (static \Dict -> Dict) `cap` cd

-- weakenClosureDict :: (Typeable y, Typeable z)
--                   => ClosureDict y -> (y :- z) -> ClosureDict z
-- weakenClosureDict y c =
--   withClosureDict y $ closure (static \Dict -> mapDict c Dict) `cap` y

-- strengthenClosureDict :: (y => z) => ClosureDict y -> ClosureDict z
-- strengthenClosureDict y =
--   withClosureDict y $ closure (static \Dict -> Dict) `cap` y

combineClosureDict :: (Typeable y, Typeable z)
                   => ClosureDict y
                   -> ClosureDict z
                   -> ClosureDict (y, z)
combineClosureDict y z =
  closure (static \Dict Dict -> Dict) `cap` y `cap` z



class StaticT y z | z -> y where
  closureDictT :: Closure (Dict y) -> Closure (Dict z)

-- instance {-# OVERLAPPING #-} (Static y, StaticT y z, z) => Static z where
--   closureDict =
--     (closureDictT :: Closure (Dict y) -> Closure (Dict z))
--     (closureDict :: Closure (Dict y))
