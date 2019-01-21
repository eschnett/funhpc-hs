{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Distributed.Closure.Instances () where

import qualified Control.Distributed.Closure as C
import qualified Control.Distributed.Closure.StaticT as C
import qualified Data.Binary as B
import qualified Type.Reflection as T

import qualified Control.Concurrent.MVar
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Complex
import qualified Data.Fixed
import qualified Data.Int
import qualified Data.IntSet
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ratio
import qualified Data.Semigroup
import qualified Data.Version
import qualified Data.Void
import qualified Data.Word
import qualified GHC.Exts
import qualified GHC.Fingerprint.Type
import qualified Numeric.Natural
import qualified Prelude
import qualified Type.Reflection
import qualified Type.Reflection.Unsafe



-- Static Binary and Static Typeable instances for basic types

instance C.Static (B.Binary ()) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.ByteString.ByteString) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.ByteString.Lazy.ByteString) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.ByteString.Short.ShortByteString) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Int.Int16) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Int.Int32) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Int.Int64) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Int.Int8) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.IntSet.IntSet) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Monoid.All) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Monoid.Any) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Version.Version) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Void.Void) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary GHC.Exts.RuntimeRep) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary GHC.Exts.VecCount) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary GHC.Exts.VecElem) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary GHC.Fingerprint.Type.Fingerprint) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Numeric.Natural.Natural) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Bool) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Char) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Double) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Float) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Int) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Integer) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Ordering) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Prelude.Word) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Word.Word16) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Word.Word32) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Word.Word64) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Data.Word.Word8) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Type.Reflection.SomeTypeRep) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Type.Reflection.TyCon) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Type.Reflection.Unsafe.KindRep) where closureDict = C.closure (static C.Dict)
instance C.Static (B.Binary Type.Reflection.Unsafe.TypeLitSort) where closureDict = C.closure (static C.Dict)

instance C.Static (T.Typeable ()) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.ByteString.ByteString) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.ByteString.Lazy.ByteString) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.ByteString.Short.ShortByteString) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Int.Int16) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Int.Int32) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Int.Int64) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Int.Int8) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.IntSet.IntSet) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Monoid.All) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Monoid.Any) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Version.Version) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Void.Void) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable GHC.Exts.RuntimeRep) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable GHC.Exts.VecCount) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable GHC.Exts.VecElem) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable GHC.Fingerprint.Type.Fingerprint) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Numeric.Natural.Natural) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Bool) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Char) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Double) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Float) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Int) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Integer) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Ordering) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Prelude.Word) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Word.Word16) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Word.Word32) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Word.Word64) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Data.Word.Word8) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Type.Reflection.SomeTypeRep) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Type.Reflection.TyCon) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Type.Reflection.Unsafe.KindRep) where closureDict = C.closure (static C.Dict)
instance C.Static (T.Typeable Type.Reflection.Unsafe.TypeLitSort) where closureDict = C.closure (static C.Dict)



-- Static Binary and Static Typeable instances for basic type constructors

instance (T.Typeable a, C.Static (B.Binary a)) => C.Static (B.Binary [a]) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary [b])
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (B.Binary a)) => C.Static (B.Binary (Data.Complex.Complex a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary (Data.Complex.Complex b))
          dict C.Dict = C.Dict
instance T.Typeable a => C.Static (B.Binary (Data.Fixed.Fixed a)) where
  closureDict = C.closure (static dict)
    where dict :: C.Dict (B.Binary (Data.Fixed.Fixed b))
          dict = C.Dict
instance (T.Typeable a, C.Static (B.Binary a)) => C.Static (B.Binary (Data.Maybe.Maybe a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary (Data.Maybe.Maybe b))
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (Prelude.Integral a), C.Static (B.Binary a)) => C.Static (B.Binary (Data.Ratio.Ratio a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict `C.cap` C.closureDict
    where dict :: C.Dict (Prelude.Integral b) -> C.Dict (B.Binary b) -> C.Dict (B.Binary (Data.Ratio.Ratio b))
          dict C.Dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (B.Binary a)) => C.Static (B.Binary (Data.Semigroup.Min a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary (Data.Semigroup.Min b))
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (B.Binary a)) => C.Static (B.Binary (Data.Semigroup.Max a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary (Data.Semigroup.Max b))
          dict C.Dict = C.Dict

instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable [a]) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable [b])
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable (Data.Complex.Complex a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Complex.Complex b))
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable (Data.Fixed.Fixed a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Fixed.Fixed b))
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable (Data.Maybe.Maybe a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Maybe.Maybe b))
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (Prelude.Integral a), C.Static (T.Typeable a)) => C.Static (T.Typeable (Data.Ratio.Ratio a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict `C.cap` C.closureDict
    where dict :: C.Dict (Prelude.Integral b) -> C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Ratio.Ratio b))
          dict C.Dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable (Data.Semigroup.Min a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Semigroup.Min b))
          dict C.Dict = C.Dict
instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable (Data.Semigroup.Max a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Semigroup.Max b))
          dict C.Dict = C.Dict



-- Static Typeable without Static Binary instances for basic type constructors

instance (T.Typeable a, C.Static (T.Typeable a)) => C.Static (T.Typeable (Control.Concurrent.MVar.MVar a)) where
  closureDict = C.closure (static dict) `C.cap` C.closureDict
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Control.Concurrent.MVar.MVar b))
          dict C.Dict = C.Dict



-- StaticT Binary and StaticT Typeable instances for basic type constructors

instance T.Typeable a => C.StaticT (B.Binary a) (B.Binary [a]) where
  closureDictT cd = C.closure (static dict) `C.cap` cd
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary [b])
          dict C.Dict = C.Dict
instance T.Typeable a => C.StaticT (B.Binary a) (B.Binary (Data.Complex.Complex a)) where
  closureDictT cd = C.closure (static dict) `C.cap`cd
    where dict :: C.Dict (B.Binary b) -> C.Dict (B.Binary (Data.Complex.Complex b))
          dict C.Dict = C.Dict

instance T.Typeable a => C.StaticT (T.Typeable a) (T.Typeable [a]) where
  closureDictT cd = C.closure (static dict) `C.cap` cd
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable [b])
          dict C.Dict = C.Dict
instance T.Typeable a => C.StaticT (T.Typeable a) (T.Typeable (Data.Complex.Complex a)) where
  closureDictT cd = C.closure (static dict) `C.cap`cd
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Data.Complex.Complex b))
          dict C.Dict = C.Dict



-- StaticT Typeable without StaticT Binary instances for basic type constructors

instance T.Typeable a => C.StaticT (T.Typeable a) (T.Typeable (Control.Concurrent.MVar.MVar a)) where
  closureDictT cd = C.closure (static dict) `C.cap` cd
    where dict :: C.Dict (T.Typeable b) -> C.Dict (T.Typeable (Control.Concurrent.MVar.MVar b))
          dict C.Dict = C.Dict
