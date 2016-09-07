{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Builder where

import Data.Text (Text)
import Control.Monad.ST (ST)
import Data.Word
import Data.Int
import Data.Monoid
import Data.Text.Internal.Builder.Int.Digits (digits)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.Text.Internal as I
import qualified Data.Text as Text
import qualified Data.Text.Array as A
import qualified Data.Text.Array as TArray
import qualified Data.Text.Internal.Unsafe.Char as C

data Builder = Builder
  { builderSize :: {-# UNPACK #-} !Int
  , builderWrite :: !(forall s. A.MArray s -> Int -> ST s Int)
  }

instance Monoid Builder where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = append
  {-# INLINE mappend #-}

empty :: Builder
empty = Builder 0 (const return)
{-# INLINE empty #-}

append :: Builder -> Builder -> Builder
append (Builder maxSizeA f) (Builder maxSizeB g) = do
  Builder (maxSizeA + maxSizeB) (\marr i -> f marr i >>= g marr)
{-# INLINE append #-}

toText :: Builder -> Text
toText x =
  let (!arr, !word16Used) = A.run2 $ case x of
        Builder maxSize f -> do
          marr <- A.new maxSize
          finalSize <- f marr 0
          return (marr,finalSize)
   in I.Text arr 0 word16Used
{-# INLINE toText #-}

singletonWord16 :: Word16 -> Builder
singletonWord16 w = Builder 1 $ \marr i -> do
  A.unsafeWrite marr i w
  return (i + 1)
{-# INLINE singletonWord16 #-}

-- singleton :: Char -> Builder
-- singleton c = Builder (\i marr -> do
--   C.unsafeWrite marr i c
--   ) 2

fromText :: Text -> Builder
fromText (I.Text arr start len) = Builder len $ \marr i -> do
  let j = len + i
  A.copyI marr i arr start j
  return j

positive :: (Integral a) => a -> Builder
{-# SPECIALIZE positive :: Int -> Builder #-}
{-# SPECIALIZE positive :: Int8 -> Builder #-}
{-# SPECIALIZE positive :: Int16 -> Builder #-}
{-# SPECIALIZE positive :: Int32 -> Builder #-}
{-# SPECIALIZE positive :: Int64 -> Builder #-}
{-# SPECIALIZE positive :: Word -> Builder #-}
{-# SPECIALIZE positive :: Word8 -> Builder #-}
{-# SPECIALIZE positive :: Word16 -> Builder #-}
{-# SPECIALIZE positive :: Word32 -> Builder #-}
{-# SPECIALIZE positive :: Word64 -> Builder #-}
positive i
    | i < 10    = Builder 1 $ \marr off -> do
                    A.unsafeWrite marr off (i2w i)
                    return (off + 1)
    | otherwise = let !n = countDigits i
                  in Builder n $ \marr off -> do
                    posDecimal marr off n i
                    return (off + n)


posDecimal :: (Integral a) =>
              forall s. A.MArray s -> Int -> Int -> a -> ST s ()
{-# INLINE posDecimal #-}
posDecimal marr off0 ds v0 = go (off0 + ds - 1) v0
  where go off v
           | v >= 100 = do
               let (q, r) = v `quotRem` 100
               write2 off r
               go (off - 2) q
           | v < 10    = A.unsafeWrite marr off (i2w v)
           | otherwise = write2 off v
        write2 off i0 = do
          let i = fromIntegral i0; j = i + i
          A.unsafeWrite marr off $ get (j + 1)
          A.unsafeWrite marr (off - 1) $ get j
        get = fromIntegral . B.unsafeIndex digits


minus, zero :: Word16
{-# INLINE minus #-}
{-# INLINE zero #-}
minus = 45
zero = 48

i2w :: (Integral a) => a -> Word16
{-# INLINE i2w #-}
i2w v = zero + fromIntegral v

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

word8Decimal :: Word8 -> Builder
word8Decimal w
  | w < 10 = Builder 1 $ \marr i -> do
      TArray.unsafeWrite marr i (i2w w)
      return (i + 1)
  | w < 100 = Builder 2 $ \marr i -> do
      write2 marr i w
      return (i + 2)
  | otherwise = Builder 3 $ \marr i -> do
      write3 marr i w
      return (i + 3)
  where
  write2 marr off i0 = do
    let i = fromIntegral i0; j = i + i
    TArray.unsafeWrite marr off $ get2 j
    TArray.unsafeWrite marr (off + 1) $ get2 (j + 1)
  write3 marr off i0 = do
    let i = fromIntegral i0; j = i + i + i
    TArray.unsafeWrite marr off $ get3 j
    TArray.unsafeWrite marr (off + 1) $ get3 (j + 1)
    TArray.unsafeWrite marr (off + 2) $ get3 (j + 2)
  get2 = fromIntegral . ByteString.unsafeIndex twoDigits
  get3 = fromIntegral . ByteString.unsafeIndex threeDigits
{-# INLINE word8Decimal #-}

twoDigits :: ByteString
twoDigits = BC8.pack
  "0001020304050607080910111213141516171819\
  \2021222324252627282930313233343536373839\
  \4041424344454647484950515253545556575859\
  \6061626364656667686970717273747576777879\
  \8081828384858687888990919293949596979899"
{-# NOINLINE twoDigits #-}

threeDigits :: ByteString
threeDigits =
  ByteString.replicate 300 0 <> BC8.pack
  "100101102103104105106107108109110111112\
  \113114115116117118119120121122123124125\
  \126127128129130131132133134135136137138\
  \139140141142143144145146147148149150151\
  \152153154155156157158159160161162163164\
  \165166167168169170171172173174175176177\
  \178179180181182183184185186187188189190\
  \191192193194195196197198199200201202203\
  \204205206207208209210211212213214215216\
  \217218219220221222223224225226227228229\
  \230231232233234235236237238239240241242\
  \243244245246247248249250251252253254255"
{-# NOINLINE threeDigits #-}
