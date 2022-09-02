{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Generic.GReadEnum where 

import GHC.Generics
import Data.Monoid
import Text.Read

class GReadEnum a where 
  greadEnum :: String -> Maybe a

  default greadEnum :: (Generic a, ReadEnum (Rep a)) 
                    => String 
                    -> Maybe a
  greadEnum = greadEnumDefault

greadEnumDefault :: (Generic a, ReadEnum (Rep a)) 
                => String 
                -> Maybe a
greadEnumDefault str = getFirst $ fmap to (readEnum str)

class ReadEnum f where 
  readEnum :: String -> First (f a)

instance ReadEnum p => ReadEnum (M1 D f p) where 
  readEnum str = fmap M1 (readEnum str)

instance (ReadEnum f, ReadEnum g) => ReadEnum (f :+: g) where 
  readEnum str = fmap L1 (readEnum str) <> fmap R1 (readEnum str)

instance (ReadEnum p, Constructor f) => ReadEnum (M1 C f p) where 
  readEnum str = 
    if str == conName x
      then fmap M1 (readEnum str)
      else mempty
    where 
      x :: M1 C f p a
      x = undefined

instance ReadEnum U1 where 
  readEnum _ = pure U1
