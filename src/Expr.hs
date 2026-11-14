{-# LANGUAGE LambdaCase, StrictData, DeriveGeneric, DeriveAnyClass #-}
module Expr (Expr (..), gen, eval) where

import Data.Ratio
import Control.Monad
import EquivRep
import Control.Applicative
import GHC.Generics (Generic)
import GHC.Generics qualified
import Control.DeepSeq (NFData (..))

data Expr
  = Lit Int
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  deriving (Eq, Ord, Generic)

instance NFData Expr

instance Show Expr where
  showsPrec p =
    \case
    Lit n -> shows n
    a :+: b -> go a b 6 0 " + "
    a :-: b -> go a b 6 1 " - "
    a :*: b -> go a b 7 0 " * "
    a :/: b -> go a b 7 1 " / "
    where go a b q q' sym =
            showParen (p > q) $ showsPrec q a . (sym <>) . showsPrec (q + q') b

gen = gen' where
  gen' [] = []
  gen' [a] = [Lit a]
  gen' xs = do
    (as, bs) <- partitions xs
    [(:+:), (:-:), (:*:), (:/:)] <*> (gen as) <*> (gen bs)

-- ways to divide a list into 2 non-empty sublists. order is not necessarily preserved.
partitions = filter isProperPartition . ($ []) . go [] [] where
  go a b = \case
    [] -> ((a,b) :)
    x:xs -> go (x : a) b xs . go a (x:b) xs

isProperPartition (a, b) = not (null a || null b)

{-# INLINABLE eval #-}
eval = \case
  Lit a -> Just (fromIntegral a :: Rational)
  a :+: b -> f a b $ \ a b -> pure $ a + b
  a :-: b -> f a b $ \ a b -> pure $ a - b
  a :*: b -> f a b $ \ a b -> pure $ a * b
  a :/: b -> f a b $ \ a b -> guard (b /= 0) *> pure (a / b)
  where f a b g = do
           a' <- eval a
           b' <- eval b
           g a' b'
