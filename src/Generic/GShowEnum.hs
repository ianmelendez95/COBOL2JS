{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Generic.GShowEnum where

import GHC.Generics

class GShowEnum f where 
  gshowEnum :: (String -> String) -> f p -> String

instance GShowEnum p => GShowEnum (M1 D f p) where 
  gshowEnum strFun (M1 x) = gshowEnum strFun x

instance (GShowEnum f, GShowEnum g) => GShowEnum (f :+: g) where 
  gshowEnum strFun (L1 x) = gshowEnum strFun x
  gshowEnum strFun (R1 x) = gshowEnum strFun x

instance (Constructor f) => GShowEnum (M1 C f p) where 
  gshowEnum strFun = strFun . conName