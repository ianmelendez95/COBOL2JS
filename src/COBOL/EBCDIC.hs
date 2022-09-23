{-# LANGUAGE OverloadedStrings #-}


module COBOL.EBCDIC 
 ( readText
 , readComp3
 , readComp3Digits
 ) where 

import System.IO

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Numeric (showHex)

import Data.Word (Word8)
import Data.Bits (Bits ((.&.), shiftR))

import Data.Ratio

-- TODO could be more performant if use BS.map and T.decodeUtf8 (possibly less readable/debuggable though)
readText :: Handle -> Int -> IO T.Text
readText h n = do 
  bytes <- BS.hGet h n
  pure . T.pack $ map getDecodedChar (BS.unpack bytes) 

-- [Reading Digits]
--
-- http://www.3480-3590-data-conversion.com/article-packed-fields.html
--
-- Per the linked article, the digits are encoded where each nibble forms a digit.
-- The last nibble is technically not a digit but rather the 'sign' nibble.
--
-- So the number 123 would be encoded as 0x123C. (0xC at the end flags it as positive, 0xD is negative)
-- More examples:
--    1,234 = 0x01234C (8-bit aligned, so notice the 0 at the beginning to pad it out)
--   -1,234 = 0x01234D
-- 
-- For numbers with a decimal point, the digits are encoded the same, and the COBOL 
-- system uses the PIC clause to determine where to insert the decimal. 
-- So for example, 12.34 would still be encoded as 0x01234C, the same as 1,234. 
readComp3 :: Handle -> Int -> Int -> IO Double
readComp3 h n whole_l = 
  readComp3Digits whole_l . concatMap decodeByteDigits . BS.unpack <$> BS.hGet h n
  where 
    decodeByteDigits :: Word8 -> [Word8]
    decodeByteDigits byte = 
      let first  = shiftR byte 4
          second = byte .&. 0x0F
      in [first, second]

-- [Parsing Digits]
--
-- http://www.3480-3590-data-conversion.com/article-packed-fields.html
--
-- Per the linked article, the last 'digit' is actually the sign byte.
-- The rest of the digits form the number, where the position of the decimal
-- is known by the PIC clause.
--
-- Approach here is to read all of the digits as if they formed the coefficient in 
-- scientific notation. This coefficient is then divided by 10 to the length of the 
-- part/fractional portion.
-- 
-- So for 123.45, we read in all of the digits as 12,345 (the coefficient).
-- We then consider that the length of the 'part', .45, is 2.
-- Then we simply calculate 12,345 / (10 ^ 2) = 123.45
--
-- This approach is used by the Read Double instance in GHC.Base
--
-- TODO consider using the 'scientific' library
readComp3Digits :: Int -> [Word8] -> Double
readComp3Digits whole_l digits =
  let (coeff_ds, sign_d) = splitAtEnd digits

      coeff_l = length coeff_ds
      part_l  = coeff_l - whole_l

      -- can safely read as is, the first byte is padded with a 0 
      -- if the number doesn't fill the bytes
      coeff   = digitsToInt coeff_ds  

      pos_value = if whole_l == coeff_l 
                    then fromIntegral coeff 
                    else fromRational $ coeff % (10 ^ part_l)
  in if sign_d == 0xD  -- 0xC = positive, 0xD = negative
       then negate pos_value
       else pos_value
  where 
    digitsToInt :: [Word8] -> Integer
    digitsToInt = foldl (\acc x -> (acc * 10) + fromIntegral x) 0

    splitAtEnd :: [a] -> ([a], a)
    splitAtEnd [] = error "empty list"
    splitAtEnd [x,y] = ([x], y)
    splitAtEnd (x:xs) = let (xs', x') = splitAtEnd xs in (x : xs', x')

getDecodedChar :: Word8 -> Char
getDecodedChar w = 
  case IntMap.lookup (fromIntegral w) decode_map of 
    Nothing -> error $ "Unable to decode: " ++ showHex w ""
    Just c  -> c

decode_map :: IntMap Char
decode_map =  IntMap.fromList 
  [ (0x40, 's')
  , (0x4B, '.')
  , (0x4E, '+')

  , (0x5B, '$')

  , (0x60, '-')
  , (0x6B, ',')
  , (0x6C, '%')
  , (0x6D, '_')

  , (0x7D, '\'')
  , (0x7E, '=')
  , (0x7F, '"')

  , (0x81, 'a')
  , (0x82, 'b')
  , (0x83, 'c')
  , (0x84, 'd')
  , (0x85, 'e')
  , (0x86, 'f')
  , (0x87, 'g')
  , (0x88, 'h')
  , (0x89, 'i')

  , (0x91, 'j')
  , (0x92, 'k')
  , (0x93, 'l')
  , (0x94, 'm')
  , (0x95, 'n')
  , (0x96, 'o')
  , (0x97, 'p')
  , (0x98, 'q')
  , (0x99, 'r')

  , (0xA1, '~')
  , (0xA2, 's')
  , (0xA3, 't')
  , (0xA4, 'u')
  , (0xA5, 'v')
  , (0xA6, 'w')
  , (0xA7, 'x')
  , (0xA8, 'y')
  , (0xA9, 'z')

  , (0xC0, '{')
  , (0xC1, 'A')
  , (0xC2, 'B')
  , (0xC3, 'C')
  , (0xC4, 'D')
  , (0xC5, 'E')
  , (0xC6, 'F')
  , (0xC7, 'G')
  , (0xC8, 'H')
  , (0xC9, 'I')

  , (0xD0, '}')
  , (0xD1, 'J')
  , (0xD2, 'K')
  , (0xD3, 'L')
  , (0xD4, 'M')
  , (0xD5, 'N')
  , (0xD6, 'O')
  , (0xD7, 'P')
  , (0xD8, 'Q')
  , (0xD9, 'R')
                 
  , (0xE0, '\\')
  , (0xE2, 'S')
  , (0xE3, 'T')
  , (0xE4, 'U')
  , (0xE5, 'V')
  , (0xE6, 'W')
  , (0xE7, 'X')
  , (0xE8, 'Y')
  , (0xE9, 'Z')

  , (0xF0, '0')
  , (0xF1, '1')
  , (0xF2, '2')
  , (0xF3, '3')
  , (0xF4, '4')
  , (0xF5, '5')
  , (0xF6, '6')
  , (0xF7, '7')
  , (0xF8, '8')
  , (0xF9, '9')
  ]