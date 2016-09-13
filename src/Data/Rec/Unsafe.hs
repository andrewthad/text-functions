{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rec.Unsafe where

import Data.Vector (Vector)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Equality
import Data.Proxy
import qualified Data.Vector as Vector

newtype Rec (f :: k -> *) (xs :: [k]) = Rec (Vector Any)

rnull :: Rec f rs -> Bool
rnull (Rec v) = Vector.null v
{-# INLINE rnull #-}

rhead :: Rec f (r ': rs) -> f r
rhead (Rec v) = unsafeCoerce (Vector.head v)
{-# INLINE rhead #-}

rsingleton :: f a -> Rec f '[a]
rsingleton a = Rec (Vector.singleton (unsafeCoerce a))
{-# INLINE rsingleton #-}

rnil :: Rec f '[]
rnil = Rec Vector.empty
{-# INLINE rnil #-}

rcons :: f r -> Rec f rs -> Rec f (r ': rs)
rcons a (Rec as) = Rec (Vector.cons (unsafeCoerce a) as)
{-# INLINE rcons #-}

rappend :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
rappend (Rec a) (Rec b) = Rec (a Vector.++ b)
{-# INLINE rappend #-}

rtraverse2_ :: Monad h => (forall x. f x -> g x -> h a) -> Rec f rs -> Rec g rs -> h ()
rtraverse2_ f (Rec as) (Rec bs) = Vector.mapM_ (\(a,b) -> f (unsafeCoerce a) (unsafeCoerce b)) (Vector.zip as bs)
{-# INLINE rtraverse2_ #-}

rmap :: (forall a. f a -> g a) -> Rec f rs -> Rec g rs
rmap f (Rec v) = Rec (Vector.map (unsafeCoerce . f . unsafeCoerce) v)

rsplit :: Rec Proxy as -> Rec Proxy bs -> Rec f (as ++ bs) -> (Rec f as, Rec f bs)
rsplit (Rec aProxies) (Rec _) (Rec both) =
  let alen = Vector.length aProxies
      (as,bs) = Vector.splitAt alen both
   in (Rec as, Rec bs)
{-# INLINE rsplit #-}

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

rtoProxy :: Rec f rs -> Rec Proxy rs
rtoProxy = rmap (const Proxy)
{-# INLINE rtoProxy #-}

appendRightIdentity :: proxy as -> ((as ++ '[]) :~: as)
appendRightIdentity = unsafeCoerce Refl
{-# INLINE appendRightIdentity #-}

