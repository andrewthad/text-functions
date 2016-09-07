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
  let ipAddr = IpAddress 192 168 0 99
  defaultMain
    [ bgroup "IPv4 to Text"
      [ bench "Strict Builder" $ whnf strictBuilder ipAddr
      , bench "Strict Builder Word8" $ whnf strictBuilderWord8 ipAddr
      , bench "Naive" $ whnf naive ipAddr
      , bench "Lazy Builder" $ whnf lazyBuilder ipAddr
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

strictBuilder :: IpAddress -> Text
strictBuilder (IpAddress a b c d) =
  StrictBuilder.toText $
     StrictBuilder.positive a
  <> dot
  <> StrictBuilder.positive b
  <> dot
  <> StrictBuilder.positive c
  <> dot
  <> StrictBuilder.positive d
  where dot = StrictBuilder.singletonWord16 46

strictBuilderWord8 :: IpAddress -> Text
strictBuilderWord8 (IpAddress a b c d) =
  StrictBuilder.toText $
     StrictBuilder.word8Decimal a
  <> dot
  <> StrictBuilder.word8Decimal b
  <> dot
  <> StrictBuilder.word8Decimal c
  <> dot
  <> StrictBuilder.word8Decimal d
  where dot = StrictBuilder.singletonWord16 46

