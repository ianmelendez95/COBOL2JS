{-# LANGUAGE TemplateHaskell #-}

module JS.Runtime
  ( runtimeText ) where

import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS

runtimeText :: T.Text
runtimeText = decodeUtf8 runtimeByteString

runtimeByteString :: BS.ByteString
runtimeByteString = $(embedFile "js/runtime.js")


