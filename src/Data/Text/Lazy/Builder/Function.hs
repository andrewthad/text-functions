{-# LANGUAGE BangPatterns #-}

module Data.Text.Lazy.Builder.Function
  ( toStrictText
  , unlines
  , unwords
  , replicate
  ) where

import Prelude hiding (unlines,unwords,replicate)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Monoid

toStrictText :: Builder -> Text
toStrictText = LText.toStrict . Builder.toLazyText

unlines :: [Builder] -> Builder
unlines = foldr (\t -> mappend t . mappend newlineBuilder) mempty

unwords :: [Builder] -> Builder
unwords = go
  where
  go [] = mempty
  go (b : bs) = mappend b (mappend spaceBuilder (go bs))

replicate :: Int -> Builder -> Builder
replicate i t = go i
  where
  go !n = if n > 0
    then mappend t (go (n - 1))
    else mempty


newlineBuilder :: Builder
newlineBuilder = Builder.singleton '\n'

spaceBuilder :: Builder
spaceBuilder = Builder.singleton ' '


