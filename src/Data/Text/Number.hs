{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Number where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Text.Internal.Fusion (Stream(..),Step(..),unstream,stream)
import Data.Text.Internal.Fusion.Common (streamList)
import Data.Text.Internal.Builder.Functions (i2d)
import Data.Text.Internal.Fusion.Size (exactSize)
import Data.Int
import Data.Word
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector

data PDI i = PDI !i {-# UNPACK #-} !Int

integralToText :: Integral a => a -> Text
integralToText i = unstream (generalPosDecimal i)
{-# INLINE integralToText #-}

generalPosDecimal :: (Integral a) => a -> Stream Char
generalPosDecimal = posDecimalWith (10 ^)
{-# RULES "generalPosDecimal Word8" generalPosDecimal = posDecimalWord8Lookup #-}
{-# NOINLINE generalPosDecimal #-}

posDecimalWord8 :: Word8 -> Stream Char
posDecimalWord8 = posDecimalWith (UVector.unsafeIndex powersOfTenWord8)
{-# INLINE posDecimalWord8 #-}

posDecimalWord8Lookup :: Word8 -> Stream Char
posDecimalWord8Lookup = Vector.unsafeIndex word8Streams . fromIntegral
{-# INLINE posDecimalWord8Lookup #-}

posDecimalWith :: (Integral a) => (Int -> a) -> a -> Stream Char
posDecimalWith tenExp a = Stream go (PDI a origDigitCount) (exactSize origDigitCount)
  where
  {-# INLINE go #-}
  go (PDI i digitCount)
    | digitCount > 0 =
        let !digitCountMinus = digitCount - 1
            (!firstDigit, !inext) = quotRem i (tenExp digitCountMinus)
         in Yield (i2d (fromIntegral firstDigit)) (PDI inext digitCountMinus)
    | otherwise = Done
  !origDigitCount = countDigits a
{-# INLINE posDecimalWith #-}

powersOfTenWord8 :: UVector.Vector Word8
powersOfTenWord8 = UVector.fromList [ 1 , 10 , 100 ]
{-# NOINLINE powersOfTenWord8 #-}

word8Streams :: Vector (Stream Char)
word8Streams = Vector.fromList (map (stream . Text.pack . show) [0 :: Word8 ..255])

powersOfTen :: (Integral a) => Vector a
powersOfTen = Vector.fromList
  [ 1
  , 10
  , 100
  , 1000
  , 10000
  , 100000
  , 1000000
  , 10000000
  , 100000000
  , 1000000000
  , 10000000000
  , 100000000000
  , 1000000000000
  , 10000000000000
  , 100000000000000
  , 1000000000000000
  , 10000000000000000
  , 100000000000000000
  , 1000000000000000000
  , 10000000000000000000
  , 100000000000000000000
  , 1000000000000000000000
  , 10000000000000000000000
  , 100000000000000000000000
  ]
{-# NOINLINE powersOfTen #-}

countDigits :: (Integral a) => a -> Int
{-# INLINE countDigits #-}
countDigits v0
  | fromIntegral v64 == v0 = go 1 v64
  | otherwise              = goBig 1 (fromIntegral v0)
  where v64 = fromIntegral v0
        goBig !k (v :: Integer)
           | v > big   = goBig (k + 19) (v `quot` big)
           | otherwise = go k (fromIntegral v)
        big = 10000000000000000000
        go !k (v :: Word64)
           | v < 10    = k
           | v < 100   = k + 1
           | v < 1000  = k + 2
           | v < 1000000000000 =
               k + if v < 100000000
                   then if v < 1000000
                        then if v < 10000
                             then 3
                             else 4 + fin v 100000
                        else 6 + fin v 10000000
                   else if v < 10000000000
                        then 8 + fin v 1000000000
                        else 10 + fin v 100000000000
           | otherwise = go (k + 12) (v `quot` 1000000000000)
        fin v n = if v >= n then 1 else 0

