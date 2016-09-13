{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rec where

import Data.Type.Equality
import Data.Proxy

data Rec (f :: k -> *) (rs :: [k]) where
  RNil :: Rec f '[]
  RCons :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

rnull :: Rec f rs -> Bool
rnull RNil = True
rnull _ = False
{-# INLINE rnull #-}

rhead :: Rec f (r ': rs) -> f r
rhead (RCons a _) = a
{-# INLINE rhead #-}

rnil :: Rec f '[]
rnil = RNil
{-# INLINE rnil #-}

rcons :: f r -> Rec f rs -> Rec f (r ': rs)
rcons = RCons
{-# INLINE rcons #-}

rtraverse_ :: Applicative h => (forall x. f x -> h a) -> Rec f rs -> h ()
rtraverse_ _ RNil = pure ()
rtraverse_ f (RCons x xs) = f x *> rtraverse_ f xs
{-# INLINABLE rtraverse_ #-}

rtraverse2_ :: Applicative h => (forall x. f x -> g x -> h a) -> Rec f rs -> Rec g rs -> h ()
rtraverse2_ _ RNil RNil = pure ()
rtraverse2_ f (RCons x xs) (RCons y ys) = f x y *> rtraverse2_ f xs ys
{-# INLINABLE rtraverse2_ #-}

rmap :: (forall a. f a -> g a) -> Rec f rs -> Rec g rs
rmap _ RNil = RNil
rmap f (RCons r rs) = RCons (f r) (rmap f rs)
{-# INLINABLE rmap #-}

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

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

appendRightIdentity :: Rec Proxy as -> ((as ++ '[]) :~: as)
appendRightIdentity RNil = Refl
appendRightIdentity (RCons _ rs) | Refl <- appendRightIdentity rs = Refl

