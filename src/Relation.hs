module Relation where

import Expr

relation = recurseRelation $ applyAll [addAssoc, addComm, mulAssoc, mulComm, subAssoc, subAntiComm, divAssoc, divAntiComm, addIdentity, mulIdentity]


-- These rules are purely structural.
addAssoc ((a :+: b) :+: c) = [a :+: (b :+: c)]
addAssoc ((a :+: b) :-: c) = [a :+: (b :-: c)]
addAssoc _ = []

subAssoc (a :-: (b :-: c)) = [(a :-: b) :+: c]
subAssoc (a :-: (b :+: c)) = [(a :-: b) :-: c]
subAssoc _ = []

addComm (a :+: b) = [b :+: a]
addComm _ = []

subAntiComm (a :+: (b :-: c)) = [a :-: (c :-: b)]
subAntiComm _ = []

mulAssoc ((a :*: b) :*: c) = [a :*: (b :*: c)]
mulAssoc ((a :*: b) :/: c) = [a :*: (b :/: c)]
mulAssoc _ = []

divAssoc (a :/: (b :/: c)) = [(a :/: b) :*: c]
divAssoc (a :/: (b :*: c)) = [(a :/: b) :/: c]
divAssoc _ = []

mulComm (a :*: b) = [b :*: a]
mulComm _ = []

divAntiComm (a :*: (b :/: c)) = [a :/: (c :/: b)]
divAntiComm _ = []

-- These rules depend on the value of a subexpression.
addIdentity (a :+: b) | eval b == Just 0 = [a :-: b]
addIdentity _ = []

mulIdentity (a :*: b) | eval b == Just 1 = [a :/: b]
mulIdentity _ = []

applyAll :: [a -> [a]] -> a -> [a]
applyAll fs = concat . sequence fs

recurseRelation r = go
  where
    go e = case e of
      Lit _ -> r e
      a :+: b -> r e <> fmap (:+:b) (go a) <> fmap (a:+:) (go b)
      a :-: b -> r e <> fmap (:-:b) (go a) <> fmap (a:-:) (go b)
      a :*: b -> r e <> fmap (:*:b) (go a) <> fmap (a:*:) (go b)
      a :/: b -> r e <> fmap (:/:b) (go a) <> fmap (a:/:) (go b)
