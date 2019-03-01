{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Distributed.Closure.Static where

import Control.Distributed.Closure hiding (Static(..))
import Data.Binary
import Type.Reflection

import qualified Control.Concurrent
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Complex
import qualified Data.Int
import qualified Data.IntSet
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Semigroup
import qualified Data.Version
import qualified Data.Void
import qualified Data.Word
import qualified GHC.Exts
import qualified GHC.Fingerprint.Type
import qualified Numeric.Natural
import qualified Type.Reflection.Unsafe



class (c, Typeable c) => Static c where
  -- dict :: Dict c
  -- dict = Dict
  closureDict :: Closure (Dict c)
  closureDictStatic :: Closure (Dict (Static c))


instance (Static c1, Static c2, Typeable c1, Typeable c2, (c1, c2)) =>
         Static (c1, c2) where
  closureDict = static dict `cap` closureDict `cap` closureDict
    where dict :: Dict c3 -> Dict c4 -> Dict (c3, c4)
          dict Dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static c3) -> Dict (Static c4) -> Dict (Static (c3, c4))
          dict Dict Dict = Dict



-- Static Binary and Static Typeable instances for basic types

instance Static (Binary ()) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.ByteString.ByteString) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.ByteString.Lazy.ByteString) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.ByteString.Short.ShortByteString) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Int.Int16) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Int.Int32) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Int.Int64) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Int.Int8) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.IntSet.IntSet) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Monoid.All) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Monoid.Any) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Word.Word16) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Word.Word32) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Word.Word64) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Word.Word8) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Version.Version) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Data.Void.Void) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary GHC.Exts.RuntimeRep) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary GHC.Exts.VecCount) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary GHC.Exts.VecElem) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary GHC.Fingerprint.Type.Fingerprint) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Numeric.Natural.Natural) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Bool) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Char) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Double) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Float) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Int) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Integer) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Ordering) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Prelude.Word) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Type.Reflection.SomeTypeRep) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Type.Reflection.TyCon) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Type.Reflection.Unsafe.KindRep) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Binary Type.Reflection.Unsafe.TypeLitSort) where
  closureDict = static Dict
  closureDictStatic = static Dict



instance Static (Typeable ()) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.ByteString.ByteString) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.ByteString.Lazy.ByteString) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.ByteString.Short.ShortByteString) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Int.Int16) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Int.Int32) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Int.Int64) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Int.Int8) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.IntSet.IntSet) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Monoid.All) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Monoid.Any) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Word.Word16) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Word.Word32) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Word.Word64) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Word.Word8) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Version.Version) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Data.Void.Void) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable GHC.Exts.RuntimeRep) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable GHC.Exts.VecCount) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable GHC.Exts.VecElem) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable GHC.Fingerprint.Type.Fingerprint) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Numeric.Natural.Natural) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Bool) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Char) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Double) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Float) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Int) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Integer) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Ordering) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Prelude.Word) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Type.Reflection.SomeTypeRep) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Type.Reflection.TyCon) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Type.Reflection.Unsafe.KindRep) where
  closureDict = static Dict
  closureDictStatic = static Dict

instance Static (Typeable Type.Reflection.Unsafe.TypeLitSort) where
  closureDict = static Dict
  closureDictStatic = static Dict



-- Static Binary and Static Typeable instances for basic type constructors

instance (Static (Typeable a), Static (Binary a)) => Static (Binary [a]) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary [b])
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary b))
               -> Dict (Static (Binary [b]))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Complex.Complex a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Complex.Complex b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary b))
               -> Dict (Static (Binary (Data.Complex.Complex b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Maybe.Maybe a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Maybe.Maybe b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary b))
               -> Dict (Static (Binary (Data.Maybe.Maybe b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Monoid.Dual a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Monoid.Dual b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Binary b))
               -> Dict (Static (Typeable b))
               -> Dict (Static (Binary (Data.Monoid.Dual b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Monoid.First a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Monoid.First b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Binary b))
               -> Dict (Static (Typeable b))
               -> Dict (Static (Binary (Data.Monoid.First b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Monoid.Last a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Monoid.Last b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Binary b))
               -> Dict (Static (Typeable b))
               -> Dict (Static (Binary (Data.Monoid.Last b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Monoid.Product a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Monoid.Product b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Binary b))
               -> Dict (Static (Typeable b))
               -> Dict (Static (Binary (Data.Monoid.Product b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Monoid.Sum a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Monoid.Sum b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Binary b))
               -> Dict (Static (Typeable b))
               -> Dict (Static (Binary (Data.Monoid.Sum b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Semigroup.Max a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Semigroup.Max b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary b))
               -> Dict (Static (Binary (Data.Semigroup.Max b)))
          dict Dict Dict = Dict

instance (Static (Typeable a), Static (Binary a)) =>
         Static (Binary (Data.Semigroup.Min a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Binary b) -> Dict (Binary (Data.Semigroup.Min b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Binary b))
               -> Dict (Static (Binary (Data.Semigroup.Min b)))
          dict Dict Dict = Dict



instance Static (Typeable a) => Static (Typeable [a]) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable [b])
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable [b]))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Complex.Complex a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Complex.Complex b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Complex.Complex b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Maybe.Maybe a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Maybe.Maybe b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Maybe.Maybe b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Monoid.Dual a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Monoid.Dual b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Monoid.Dual b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Monoid.First a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Monoid.First b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Monoid.First b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Monoid.Last a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Monoid.Last b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Monoid.Last b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Monoid.Product a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Monoid.Product b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Monoid.Product b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Monoid.Sum a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Monoid.Sum b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Monoid.Sum b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Semigroup.Max a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Semigroup.Max b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Semigroup.Max b)))
          dict Dict = Dict

instance Static (Typeable a) =>
         Static (Typeable (Data.Semigroup.Min a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Semigroup.Min b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Semigroup.Min b)))
          dict Dict = Dict



-- Static Typeable without Static Binary instances for basic type constructors

instance Static (Typeable a) =>
         Static (Typeable (Control.Concurrent.MVar a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b)
               -> Dict (Typeable (Control.Concurrent.MVar b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Control.Concurrent.MVar b)))
          dict Dict = Dict

instance Static (Typeable a) => Static (Typeable (Data.Monoid.Endo a)) where
  closureDict = static dict `cap` closureDict
    where dict :: Dict (Typeable b) -> Dict (Typeable (Data.Monoid.Endo b))
          dict Dict = Dict
  closureDictStatic =
    static dict `cap` closureDictStatic
    where dict :: Dict (Static (Typeable b))
               -> Dict (Static (Typeable (Data.Monoid.Endo b)))
          dict Dict = Dict
