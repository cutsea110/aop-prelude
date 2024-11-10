{-# LANGUAGE NoImplicitPrelude #-}
module Combinatorial
  ( subseqs
  , partitions
  , perms
  , consl, consr
  , cup
  , interleave
  ) where

import AOPPrelude
import Data.List ((\\))

subseqs :: [a] -> [[a]]
subseqs = catalist (e, f)
  where
    e = wrap []
    f = cat . pair (list cons . cpr, outr)

new :: (a, [[a]]) -> [[a]]
new = cons . cross (wrap, id)

glues :: (a, [[a]]) -> [[[a]]]
glues (a, [])   = []
glues (a, x:xs) = [(a:x):xs]

partitions :: [a] -> [[[a]]]
partitions = catalist (e, f)
  where
    e = wrap []
    f = concat . list (cons . pair (new, glues)) . cpr

adds :: (a, [a]) -> [[a]]
adds (a, x) = [y ++ [a] ++ z | (y, z) <- splits x]

perms :: [a] -> [[a]]
perms = catalist (e, f)
  where
    e = wrap []
    f = concat . list adds . cpr

consl :: (a, ([a], b)) -> ([a], b)
consl (a, (x, y)) = (a:x, y)

consr :: (a, (b, [a])) -> (b, [a])
consr (a, (x, y)) = (x, a:y)

cup :: ([a], [a]) -> [a]
cup = uncurry (++)

interleave :: [a] -> [([a], [a])]
interleave = catalist (e, f)
  where
    e = wrap nilp
    f = cup . pair (list consl, list consr) . cpr
    nilp = ([], [])

isEqual :: Eq a => [a] -> [a] -> Bool
xs `isEqual` ys = null (xs \\ ys) && null (ys \\ xs)

elem :: Eq a => [a] -> [[a]] -> Bool
elem x = catalist (e, f)
  where
    e = False
    f (y, b) = b || y `isEqual` x
