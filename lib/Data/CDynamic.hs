module Data.CDynamic
  ( CDynamic(..)
  , toCDyn
  , fromCDyn
  , fromCDynamic
  ) where

import Type.Reflection



-- | 'Data.Dynamic' with constraints
data CDynamic z where
  CDynamic :: forall z a. (Typeable a, z a) => TypeRep a -> a -> CDynamic z

instance Show (CDynamic z) where
  showsPrec _ (CDynamic tx _) =
    showString "<<" . showsPrec 0 tx . showString ">>"

toCDyn :: forall z a. (Typeable a, z a) => a -> CDynamic z
toCDyn x = CDynamic typeRep x

fromCDyn :: forall a z. Typeable a => CDynamic z -> a -> a
fromCDyn (CDynamic tx x) x0
  | Just HRefl <- tx `eqTypeRep` rep = x
  | otherwise                        = x0
  where rep = typeRep :: TypeRep a

fromCDynamic :: forall a z. Typeable a => CDynamic z -> Maybe a
fromCDynamic (CDynamic tx x)
  | Just HRefl <- tx `eqTypeRep` rep = Just x
  | otherwise                        = Nothing
  where rep = typeRep :: TypeRep a
