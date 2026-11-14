{-# LANGUAGE LambdaCase, StrictData #-}
module Expr (Expr (..), gen, eval) where

import Data.Ratio
import Control.Monad
import GHC.Utils.Misc qualified
import Data.MemoTrie
import EquivRep

data Expr
  = Lit Int
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  deriving (Eq, Ord)

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
    a <- gen as
    b <- gen bs
    op <- [(:+:), (:-:), (:*:), (:/:)]
    pure $ a `op` b

-- partition elements into 2 sublists with at least 1 element each. order not guaranteed to be preserved.
partitions = filter (\(a,b) -> not (null a) && not (null b)) . go [] [] where
  go a b = \case
    [] -> [(a,b)]
    x:xs -> go (x:a) b xs <> go a (x:b) xs

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
