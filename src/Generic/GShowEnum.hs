{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Generic.GShowEnum where

import GHC.Generics

class GShowEnum f where 
  gshowEnum :: f p -> String

instance GShowEnum p => GShowEnum (M1 D f p) where 
  gshowEnum (M1 x) = gshowEnum x

instance (GShowEnum f, GShowEnum g) => GShowEnum (f :+: g) where 
  gshowEnum (L1 x) = gshowEnum x
  gshowEnum (R1 x) = gshowEnum x

instance (Constructor f) => GShowEnum (M1 C f p) where 
  gshowEnum = conName