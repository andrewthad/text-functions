module Main (main) where

import Criterion.Main
import Data.Bits ((.&.),(.|.),shiftR,shiftL,complement)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as LazyBuilder
import qualified Data.Text.Lazy.Builder.Int as LazyBuilder
import qualified Data.Text.Builder as StrictBuilder
import Data.Text.Internal (Text(..))
import Data.Word
import Data.ByteString (ByteString)
import Control.Monad.ST
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString

main :: IO ()
main = do
  let a = IpAddress 192 168 0 99
      b = IpAddress 192 168 0 76
      c = IpAddress 10 10 50 200
      d = IpAddress 100 0 10 190
      e = IpAddress 255 255 255 255
      f = IpAddress 10 0 0 0
  defaultMain
    [ bgroup "Multiple IPs to Text"
      [ bench "Strict Builder" $ whnf (sixIps strictBuilder) (a,b,c,d,e,f)
      , bench "Strict Builder Word8" $ whnf (sixIps strictBuilderWord8) (a,b,c,d,e,f)
      ]
    , bgroup "IPv4 to Text"
      [ bench "Strict Builder" $ whnf strictBuilderText a
      , bench "Strict Builder Word8" $ whnf strictBuilderWord8Text a
      , bench "Naive" $ whnf naive a
      , bench "Lazy Builder" $ whnf lazyBuilder a
      ]
    ]

data IpAddress = IpAddress
  { ipA :: {-# UNPACK #-} !Word8
  , ipB :: {-# UNPACK #-} !Word8
  , ipC :: {-# UNPACK #-} !Word8
  , ipD :: {-# UNPACK #-} !Word8
  }

naive :: IpAddress -> Text
naive (IpAddress a b c d) = Text.pack $ concat
  [ show a
  , "."
  , show b
  , "."
  , show c
  , "."
  , show d
  ]

lazyBuilder :: IpAddress -> Text
lazyBuilder (IpAddress a b c d) =
  LText.toStrict $ LazyBuilder.toLazyText $
     LazyBuilder.decimal a
  <> dot
  <> LazyBuilder.decimal b
  <> dot
  <> LazyBuilder.decimal c
  <> dot
  <> LazyBuilder.decimal d
  where dot = LazyBuilder.singleton '.'

strictBuilderText :: IpAddress -> Text
strictBuilderText (IpAddress a b c d) =
  StrictBuilder.toText $
     StrictBuilder.positive a
  <> dot
  <> StrictBuilder.positive b
  <> dot
  <> StrictBuilder.positive c
  <> dot
  <> StrictBuilder.positive d
  where dot = StrictBuilder.singletonWord16 46

strictBuilder :: IpAddress -> StrictBuilder.Builder
strictBuilder (IpAddress a b c d) =
     StrictBuilder.positive a
  <> dot
  <> StrictBuilder.positive b
  <> dot
  <> StrictBuilder.positive c
  <> dot
  <> StrictBuilder.positive d
  where dot = StrictBuilder.singletonWord16 46

strictBuilderWord8Text :: IpAddress -> Text
strictBuilderWord8Text (IpAddress a b c d) =
  StrictBuilder.toText $
     StrictBuilder.word8Decimal a
  <> dot
  <> StrictBuilder.word8Decimal b
  <> dot
  <> StrictBuilder.word8Decimal c
  <> dot
  <> StrictBuilder.word8Decimal d
  where dot = StrictBuilder.singletonWord16 46

strictBuilderWord8 :: IpAddress -> StrictBuilder.Builder
strictBuilderWord8 (IpAddress a b c d) =
     StrictBuilder.word8Decimal a
  <> dot
  <> StrictBuilder.word8Decimal b
  <> dot
  <> StrictBuilder.word8Decimal c
  <> dot
  <> StrictBuilder.word8Decimal d
  where dot = StrictBuilder.singletonWord16 46

sixIps :: (IpAddress -> StrictBuilder.Builder)
  -> (IpAddress,IpAddress,IpAddress,IpAddress,IpAddress,IpAddress)
  -> Text
sixIps g (a,b,c,d,e,f) = StrictBuilder.toText $
     g a <> StrictBuilder.singletonWord16 44
  <> g b <> StrictBuilder.singletonWord16 44
  <> g c <> StrictBuilder.singletonWord16 44
  <> g d <> StrictBuilder.singletonWord16 44
  <> g e <> StrictBuilder.singletonWord16 44
  <> g f
