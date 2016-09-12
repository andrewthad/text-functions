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
import Data.Text.Number (integralToText)
import Data.Functor.Identity
import Data.Text.Stitch (applyStitchF, macAddressStitch, Rec(..))
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
      r = ( RCons (Identity 44) $ RCons (Identity 12) $ RCons (Identity 87)
          $ RCons (Identity 58) $ RCons (Identity 99) $ RCons (Identity 98)
          $ RNil
          )
  defaultMain
    [ -- bgroup "Multiple IPs to Text"
      -- [ bench "Strict Builder" $ whnf (sixIps strictBuilder) (a,b,c,d,e,f)
      -- , bench "Strict Builder Word8" $ whnf (sixIps strictBuilderWord8) (a,b,c,d,e,f)
      -- , bench "Strict Builder" $ whnf strictBuilderText a
      -- , bench "Strict Builder Word8" $ whnf strictBuilderWord8Text a
      -- , bench "Naive" $ whnf naive a
      -- , bench "Lazy Builder" $ whnf lazyBuilder a
      -- , bench "String Show with Text Fusion" $ whnf betterShow a
      -- , bench "Integral to Stream" $ whnf showStreamIntegral a
      -- , bench "Integral to Stream List" $ whnf showStreamIntegralList a
      -- ]
      -- bgroup "Text Append Fusion"
      -- [ bench "Five Chars" $ whnf fiveChars 'x'
      -- , bench "Ten Chars" $ whnf tenChars 'x'
      -- , bench "Ten Chars (left-associated append)" $ whnf tenCharsAssociateLeft 'x'
      -- , bench "Twenty Chars" $ whnf twentyChars 'x'
      -- , bench "Twenty Chars (left-associated append)" $ whnf twentyCharsAssociateLeft 'x'
      -- ]
      bgroup "Text Stitching"
      [ bench "MAC Address" $ whnf (applyStitchF macAddressStitch) r
      ]
    ]



fiveChars :: Char -> Text
fiveChars =
    mappend (Text.singleton 'a')
  . mappend (Text.singleton 'b')
  . mappend (Text.singleton 'c')
  . mappend (Text.singleton 'd')
  . Text.singleton

tenChars :: Char -> Text
tenChars =
    mappend (Text.singleton 'a')
  . mappend (Text.singleton 'b')
  . mappend (Text.singleton 'c')
  . mappend (Text.singleton 'd')
  . mappend (Text.singleton 'e')
  . mappend (Text.singleton 'f')
  . mappend (Text.singleton 'g')
  . mappend (Text.singleton 'h')
  . mappend (Text.singleton 'i')
  . Text.singleton

tenCharsAssociateLeft :: Char -> Text
tenCharsAssociateLeft a = (Text.singleton 'a')
  `mappend` (Text.singleton 'b')
  `mappend` (Text.singleton 'c')
  `mappend` (Text.singleton 'd')
  `mappend` (Text.singleton 'e')
  `mappend` (Text.singleton 'f')
  `mappend` (Text.singleton 'g')
  `mappend` (Text.singleton 'h')
  `mappend` (Text.singleton 'i')
  `mappend` (Text.singleton a)

twentyChars :: Char -> Text
twentyChars =
    mappend (Text.singleton 'a')
  . mappend (Text.singleton 'b')
  . mappend (Text.singleton 'c')
  . mappend (Text.singleton 'd')
  . mappend (Text.singleton 'e')
  . mappend (Text.singleton 'f')
  . mappend (Text.singleton 'g')
  . mappend (Text.singleton 'h')
  . mappend (Text.singleton 'i')
  . mappend (Text.singleton 'j')
  . mappend (Text.singleton 'k')
  . mappend (Text.singleton 'l')
  . mappend (Text.singleton 'm')
  . mappend (Text.singleton 'n')
  . mappend (Text.singleton 'o')
  . mappend (Text.singleton 'p')
  . mappend (Text.singleton 'q')
  . mappend (Text.singleton 'r')
  . mappend (Text.singleton 's')
  . Text.singleton

twentyCharsAssociateLeft :: Char -> Text
twentyCharsAssociateLeft a = (Text.singleton 'a')
  `mappend` (Text.singleton 'b')
  `mappend` (Text.singleton 'c')
  `mappend` (Text.singleton 'd')
  `mappend` (Text.singleton 'e')
  `mappend` (Text.singleton 'f')
  `mappend` (Text.singleton 'g')
  `mappend` (Text.singleton 'h')
  `mappend` (Text.singleton 'i')
  `mappend` (Text.singleton 'b')
  `mappend` (Text.singleton 'c')
  `mappend` (Text.singleton 'd')
  `mappend` (Text.singleton 'e')
  `mappend` (Text.singleton 'f')
  `mappend` (Text.singleton 'g')
  `mappend` (Text.singleton 'h')
  `mappend` (Text.singleton 'i')
  `mappend` (Text.singleton 'h')
  `mappend` (Text.singleton 'i')
  `mappend` (Text.singleton a)

data IpAddress = IpAddress
  { ipA :: {-# UNPACK #-} !Word8
  , ipB :: {-# UNPACK #-} !Word8
  , ipC :: {-# UNPACK #-} !Word8
  , ipD :: {-# UNPACK #-} !Word8
  }

-- showStreamIntegral :: IpAddress -> Text
-- showStreamIntegral (IpAddress a b c d) =
--      integralToText a
--   <> Text.singleton '.'
--   <> integralToText b
--   <> Text.singleton '.'
--   <> integralToText c
--   <> Text.singleton '.'
--   <> integralToText d
--
-- showStreamIntegralList :: IpAddress -> Text
-- showStreamIntegralList (IpAddress a b c d) = Text.concat
--   [ integralToText a
--   , Text.singleton '.'
--   , integralToText b
--   , Text.singleton '.'
--   , integralToText c
--   , Text.singleton '.'
--   , integralToText d
--   ]

-- betterShow :: IpAddress -> Text
-- betterShow (IpAddress a b c d) =
--      Text.pack (show a)
--   <> Text.singleton '.'
--   <> Text.pack (show b)
--   <> Text.singleton '.'
--   <> Text.pack (show c)
--   <> Text.singleton '.'
--   <> Text.pack (show d)

-- naive :: IpAddress -> Text
-- naive (IpAddress a b c d) = Text.pack $ concat
--   [ show a
--   , "."
--   , show b
--   , "."
--   , show c
--   , "."
--   , show d
--   ]
--
-- lazyBuilder :: IpAddress -> Text
-- lazyBuilder (IpAddress a b c d) =
--   LText.toStrict $ LazyBuilder.toLazyText $
--      LazyBuilder.decimal a
--   <> dot
--   <> LazyBuilder.decimal b
--   <> dot
--   <> LazyBuilder.decimal c
--   <> dot
--   <> LazyBuilder.decimal d
--   where dot = LazyBuilder.singleton '.'
--
-- strictBuilderText :: IpAddress -> Text
-- strictBuilderText (IpAddress a b c d) =
--   StrictBuilder.toText $
--      StrictBuilder.positive a
--   <> dot
--   <> StrictBuilder.positive b
--   <> dot
--   <> StrictBuilder.positive c
--   <> dot
--   <> StrictBuilder.positive d
--   where dot = StrictBuilder.singletonWord16 46
--
-- strictBuilder :: IpAddress -> StrictBuilder.Builder
-- strictBuilder (IpAddress a b c d) =
--      StrictBuilder.positive a
--   <> dot
--   <> StrictBuilder.positive b
--   <> dot
--   <> StrictBuilder.positive c
--   <> dot
--   <> StrictBuilder.positive d
--   where dot = StrictBuilder.singletonWord16 46
--
-- strictBuilderWord8Text :: IpAddress -> Text
-- strictBuilderWord8Text (IpAddress a b c d) =
--   StrictBuilder.toText $
--      StrictBuilder.word8Decimal a
--   <> dot
--   <> StrictBuilder.word8Decimal b
--   <> dot
--   <> StrictBuilder.word8Decimal c
--   <> dot
--   <> StrictBuilder.word8Decimal d
--   where dot = StrictBuilder.singletonWord16 46
--
-- strictBuilderWord8 :: IpAddress -> StrictBuilder.Builder
-- strictBuilderWord8 (IpAddress a b c d) =
--      StrictBuilder.word8Decimal a
--   <> dot
--   <> StrictBuilder.word8Decimal b
--   <> dot
--   <> StrictBuilder.word8Decimal c
--   <> dot
--   <> StrictBuilder.word8Decimal d
--   where dot = StrictBuilder.singletonWord16 46
--
-- sixIps :: (IpAddress -> StrictBuilder.Builder)
--   -> (IpAddress,IpAddress,IpAddress,IpAddress,IpAddress,IpAddress)
--   -> Text
-- sixIps g (a,b,c,d,e,f) = StrictBuilder.toText $
--      g a <> StrictBuilder.singletonWord16 44
--   <> g b <> StrictBuilder.singletonWord16 44
--   <> g c <> StrictBuilder.singletonWord16 44
--   <> g d <> StrictBuilder.singletonWord16 44
--   <> g e <> StrictBuilder.singletonWord16 44
--   <> g f
