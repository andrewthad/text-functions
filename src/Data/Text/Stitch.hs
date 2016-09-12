{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Text.Stitch where

import qualified Text.Printf
import Data.Functor.Identity
import Data.Monoid
import Data.Word
import Data.Char (ord)
import Data.Vector (Vector)
import Data.Proxy
import Control.Monad.Primitive
import Data.Type.Equality
import Debug.Trace
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as MUVector

-- data Encoding
--   = Utf8
--   | Utf16
--   | Utf32

newtype Constant a (b :: k) = Constant {getConstant :: a}


newtype PackedText = PackedText { getPackedText :: UVector.Vector Word8 }
  deriving (Monoid)

defPackedText :: ExactSize -> PackedText
defPackedText (ExactSize n) = PackedText (UVector.replicate n 0)

packedTextLength :: PackedText -> Int
packedTextLength (PackedText x) = UVector.length x

newtype ExactSize = ExactSize Int
data Size = Size
  { sizeLowerBound :: Maybe Int
  , sizeUpperBound :: Maybe Int
  } deriving (Show)

getExactSize :: Size -> Maybe ExactSize
getExactSize (Size (Just a) (Just b)) = if a == b then Just (ExactSize a) else Nothing
getExactSize _ = Nothing

data IndexedHole a = IndexedHole Int ExactSize (a -> PackedText)

-- data IndexedDynamicHole a = IndexedHole Int ExactSize (a -> PackedText)

moveRightBy :: Int -> IndexedHole a -> IndexedHole a
moveRightBy n (IndexedHole a b c) = IndexedHole (a + n) b c

data Rec (f :: k -> *) (rs :: [k]) where
  RNil :: Rec f '[]
  RCons :: f r -> Rec f rs -> Rec f (r ': rs)

rtraverse_ :: Applicative h => (forall x. f x -> h a) -> Rec f rs -> h ()
rtraverse_ _ RNil = pure ()
rtraverse_ f (RCons x xs) = f x *> rtraverse_ f xs

rtraverse2_ :: Applicative h => (forall x. f x -> g x -> h a) -> Rec f rs -> Rec g rs -> h ()
rtraverse2_ _ RNil RNil = pure ()
rtraverse2_ f (RCons x xs) (RCons y ys) = f x y *> rtraverse2_ f xs ys

rmap :: (forall a. f a -> g a) -> Rec f rs -> Rec g rs
rmap _ RNil = RNil
rmap f (RCons r rs) = RCons (f r) (rmap f rs)

rtoProxy :: Rec f rs -> Rec Proxy rs
rtoProxy = rmap (const Proxy)

rappend :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
rappend RNil bs = bs
rappend (RCons a as) bs = RCons a (rappend as bs)

rsplit :: Rec Proxy as -> Rec Proxy bs -> Rec f (as ++ bs) -> (Rec f as, Rec f bs)
rsplit RNil _ rs = (RNil, rs)
rsplit (RCons _ asNext) bs (RCons a rsNext) =
  let (asRes,bsRes) = rsplit asNext bs rsNext
   in (RCons a asRes, bsRes)

-- data Stitch (xs :: [*]) where
--   Static :: ExactSize -> PackedText -> Stitch '[]
--   Hole :: Size -> (x -> PackedText) -> Stitch '[x]
--   StaticHoles :: Int -> PackedText -> Rec IndexedHole xs -> Stitch xs
--   Append :: Stitch xs -> Stitch ys -> Stitch (xs ++ ys)

data StitchF (r :: [*] -> *) (xs :: [*]) where
  Static :: PackedText -> StitchF r '[]
  Hole :: Size -> (x -> PackedText) -> StitchF r '[x]
  StaticHoles :: PackedText -> Rec IndexedHole xs -> StitchF r xs
  -- DynamicHoles :: Rec IndexedDynamicHole xs -> StitchF r xs
  Append :: r xs -> r ys -> StitchF r (xs ++ ys)

reifyStitchF :: StitchF (Rec Proxy) xs -> Rec Proxy xs
reifyStitchF (Static _) = RNil
reifyStitchF (Hole _ _) = RCons Proxy RNil
reifyStitchF (Append a b) = rappend a b
reifyStitchF (StaticHoles _ r) = rtoProxy r

reifyStitch :: HFix StitchF xs -> Rec Proxy xs
reifyStitch = hcata reifyStitchF

-- append :: HFix StitchF as -> HFix StitchF bs -> HFix StitchF (as ++ bs)
-- append

newtype Fix f = Fix {runFix :: f (Fix f)}
newtype HFix (h :: (k -> *) -> k -> *) (a :: k) = HFix { runHFix :: h (HFix h) a }

class HFunctor (h :: (k -> *) -> k -> *) where
  hfmap :: (forall a. f a -> g a) -> (forall a. h f a -> h g a)

instance HFunctor StitchF where
  hfmap _ (Static t) = Static t
  hfmap _ (Hole sz g) = Hole sz g
  hfmap _ (StaticHoles backdrop fillers) = StaticHoles backdrop fillers
  hfmap f (Append a b) = Append (f a) (f b)

printStitch :: HFix StitchF a -> IO ()
printStitch = mapM_ putStrLn . getConstant . hcata showStitchF

showStitchF :: StitchF (Constant [String]) a -> Constant [String] a
showStitchF (Static (PackedText v)) = Constant ["Static: " ++ show v]
showStitchF (Hole sz _) = Constant ["Hole: " ++ show sz]
showStitchF (StaticHoles (PackedText t) _) = Constant ["Static Holes: " ++ show t]
showStitchF (Append (Constant a) (Constant b)) =
  Constant $ ["Append"] ++ map ("--" ++) a ++ map ("--" ++) b

optimizeStitch :: HFix StitchF a -> HFix StitchF a
optimizeStitch = hcata optimizeStitchF

optimizeStitchF :: StitchF (HFix StitchF) a -> HFix StitchF a
optimizeStitchF (Static t) = HFix (Static t)
optimizeStitchF (Hole sz f) = case getExactSize sz of
  Nothing -> HFix (Hole sz f)
  Just exSz -> HFix $ StaticHoles (defPackedText exSz) (RCons (IndexedHole 0 exSz f) RNil)
optimizeStitchF (StaticHoles t f) = HFix (StaticHoles t f)
optimizeStitchF (Append (HFix a) (HFix b)) = case (a,b) of
  (Static t1, Static t2) -> HFix (Static (t1 <> t2))
  (Static t1, Hole sz g) -> case getExactSize sz of
    Nothing -> HFix (Append (HFix a) (HFix b))
    Just exSz -> HFix $ StaticHoles (t1 <> defPackedText exSz) (RCons (IndexedHole (packedTextLength t1) exSz g) RNil)
  (Static t1, StaticHoles t2 fillers) -> HFix $ StaticHoles (t1 <> t2) (rmap (moveRightBy (packedTextLength t1)) fillers)
  (Static t1, Append c d) -> HFix $ Append (HFix (Static t1)) (HFix (Append c d))
  (StaticHoles t1 fillers, Static t2) -> case appendRightIdentity (rtoProxy fillers) of
    Refl -> HFix $ StaticHoles (t1 <> t2) fillers
  (StaticHoles t1 fillers1, StaticHoles t2 fillers2) ->
    HFix $ StaticHoles (t1 <> t2) (rappend fillers1 $ rmap (moveRightBy (packedTextLength t1)) fillers2)
  -- (Hole szA f, Hole szB g) -> case getExactSize sz of
  --   Nothing -> HFix (Append (HFix a) (HFix b))
  --   Just exSz -> HFix $ StaticHoles (t1 <> defPackedText exSz) (RCons (IndexedHole (packedTextLength t1) exSz g) RNil)

applyStitchF :: HFix StitchF rs -> Rec Identity rs -> PackedText
applyStitchF (HFix x) r = case x of
  Static t -> t
  Hole sz g -> case r of
    RCons (Identity val) RNil -> g val
  Append a b ->
    let (ap,bp) = (reifyStitch a, reifyStitch b)
        (valsA, valsB) = rsplit ap bp r
     in applyStitchF a valsA <> applyStitchF b valsB
  StaticHoles (PackedText t) fillers -> PackedText $ UVector.modify (placeText fillers r) t

placeText :: PrimMonad m => Rec IndexedHole rs -> Rec Identity rs -> UVector.MVector (PrimState m) Word8 -> m ()
placeText a b buffer = rtraverse2_
  (\(IndexedHole ix (ExactSize exSz) f) (Identity v) ->
    UVector.unsafeCopy
      (MUVector.unsafeTake exSz $ MUVector.unsafeDrop ix $ buffer)
      (getPackedText $ f v)
  ) a b

evalStitch :: HFix StitchF a -> PackedText
evalStitch = getConstant . hcata evalStitchF

evalStitchF :: StitchF (Constant PackedText) a -> Constant PackedText a
evalStitchF (Static t) = Constant t
evalStitchF (Append a b) = Constant $ getConstant a <> getConstant b
evalStitchF (StaticHoles t RNil) = Constant t
evalStitchF (StaticHoles t (RCons _ _)) = error "figure out how to stop this"
evalStitchF (Hole _ _) = error "figure out how to prevent this"


hcata :: HFunctor h => (forall a. h f a -> f a) -> (forall a. HFix h a -> f a)
hcata alg = alg . hfmap (hcata alg) . runHFix

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

appendRightIdentity :: Rec Proxy as -> ((as ++ '[]) :~: as)
appendRightIdentity RNil = Refl
appendRightIdentity (RCons _ rs) | Refl <- appendRightIdentity rs = Refl

type family ToFunction (as :: [*]) where
  ToFunction '[] = PackedText
  ToFunction (a ': as) = a -> ToFunction as

-- toFunction :: HFix StitchF xs -> ToFunction xs
-- toFunction

macAddressStitch :: HFix StitchF '[Word8,Word8,Word8,Word8,Word8,Word8]
macAddressStitch = optimizeStitch
  $ append word8Hex
  $ append (staticChar ':')
  $ append word8Hex
  $ append (staticChar ':')
  $ append word8Hex
  $ append (staticChar ':')
  $ append word8Hex
  $ append (staticChar ':')
  $ append word8Hex
  $ append (staticChar ':')
  $ word8Hex


word8Hex :: HFix StitchF '[Word8]
word8Hex = HFix $ Hole
  (Size (Just 2) (Just 2))
  (\w -> Vector.unsafeIndex twoHexDigits (fromIntegral w))

staticChar :: Char -> HFix StitchF '[]
staticChar c = HFix $ Static $ PackedText $ UVector.singleton $ fromIntegral $ ord c

append :: HFix StitchF as -> HFix StitchF bs -> HFix StitchF (as ++ bs)
append a b = HFix $ Append a b

twoHexDigits :: Vector PackedText
twoHexDigits = id
  $ Vector.map (\w -> PackedText $ UVector.fromList $ map (fromIntegral . ord) $ Text.Printf.printf "%02x" w)
  $ Vector.enumFromTo 0 (255 :: Word)
