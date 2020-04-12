{-# LANGUAGE NoImplicitPrelude #-}
module AOPPrelude
  ( -- Standard combinators
    (.), const, id
  , outl, outr, swap
  , assocl, assocr
  , dupl, dupr
  , pair, cross, cond
  , curry, uncurry
    -- Boolean functions
  , false, true
  , (&&)
  , (||)
  , not
  , otherwise
    -- Relations
  , leq, less, eql, neq, gtr, geq
  , meet, join, wok
    -- Numerical functions
  , zero, succ, pred
  , plus, minus, times, divide
  , negative, positive
    -- List-processing functions
  , (++)
  , null
  , nil, wrap, cons, cat, concat, snoc
  , head, tail, split
  , last, init
  , inits, tails, splits
  , cpp, cpl, cpr, cplist
  , minlist, bmin
  , maxlist, bmax
  , thinlist
  , length, sum, trans, list, filter
  , catalist
  , cata1list
  , cata2list
  , loop
  , merge
  , zip
  , unzip
    -- Word and line processing functions
  , words
  , lines
  , unwords
  , unlines
    -- Essentials and built-in primitives
  , ord, chr
  , (==), (/=), (<=), (<), (>=), (>)
  , (+), (-), (/), div, mod, (*)
  , negate, primPrint, strict, error
  , show
  , flip
    -- Re-exports
  , String
  , Num
  , Fractional
  , Show
  , Natural
  , module GHC.Types
  ) where
---------------------------------------------------------------------
-- Prelude for `Algebra of Programming' -----------------------------
-- Original created 14 Sept, 1995, by Richard Bird ------------------
---------------------------------------------------------------------

-- Operator precedence table: ---------------------------------------
import GHC.Base ((==), (/=), (<), (<=), (>=), (>), ($!), String)
import GHC.Err (error)
import GHC.Num ((+), (-), (*), negate, Num)
import GHC.Real ((/), div, mod, Fractional)
import GHC.Show (Show, show)
import GHC.Classes hiding (not, (&&), (||))
import GHC.Types

import Numeric.Natural (Natural)
import Data.Char (ord, chr)
import System.IO (print)

infixr 9 .
infixr 5 ++
infixr 3 &&
infixr 2 ||

-- Standard combinators: --------------------------------------------
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
const :: a -> b -> a
const k a = k
id :: a -> a
id a      = a

outl :: (a, b) -> a
outl (a, _) = a
outr :: (a, b) -> b
outr (_, b) = b
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
assocr :: ((a, b), c) -> (a, (b, c))
assocr ((a, b), c) = (a, (b, c))

dupl :: (a, (b, c)) -> ((a, b), (a, c))
dupl (a, (b, c)) = ((a, b), (a, c))
dupr :: ((a, b), c) -> ((a, c), (b, c))
dupr ((a, b), c) = ((a, c), (b, c))

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) a       = (f a, g a)
cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (a, b) = (f a, g b)
cond :: (a -> Bool) -> (a -> b, a -> b) -> a -> b
cond p (f, g) a     = if p a then f a else g a

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b      = f (a, b)
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

-- Boolean functions: -----------------------------------------------
false :: a -> Bool
false = const False
true  :: a -> Bool
true  = const True

(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && x = x

(||) :: Bool -> Bool -> Bool
False || x = x
True  || _ = True

not :: Bool -> Bool
not True   = False
not False  = True

otherwise :: Bool
otherwise  = True

-- Relations: -------------------------------------------------------
leq :: Ord a => (a, a) -> Bool
leq  = uncurry (<=)
less :: Ord a => (a, a) -> Bool
less = uncurry (<)
eql :: Ord a => (a, a) -> Bool
eql  = uncurry (==)
neq :: Ord a => (a, a) -> Bool
neq  = uncurry (/=)
gtr :: Ord a => (a, a) -> Bool
gtr  = uncurry (>)
geq :: Ord a => (a, a) -> Bool
geq  = uncurry (>=)

meet :: (a -> Bool, a -> Bool) -> a -> Bool
meet (r, s) = cond r (s, false)
join :: (a -> Bool, a -> Bool) -> a -> Bool
join (r, s) = cond r (true, s)
wok :: ((b, a) -> c) -> (a, b) -> c
wok r       = r . swap

-- Numerical functions: ---------------------------------------------
zero   :: Num a => t -> a
zero   = const 0
succ   :: Num a => a -> a
succ   = (+1)
pred   :: Num a => a -> a
pred n = n - 1

plus   :: Num a => (a, a) -> a
plus   = uncurry (+)
minus  :: Num a => (a, a) -> a
minus  = uncurry (-)
times  :: Num a => (a, a) -> a
times  = uncurry (*)
divide :: Fractional a => (a, a) -> a
divide = uncurry (/)

negative :: (Ord a, Num a) => a -> Bool
negative = (< 0)
positive :: (Ord a, Num a) => a -> Bool
positive = (> 0)

-- List-processing functions: ---------------------------------------
(++) :: [a] -> [a] -> [a]
[] ++ y    = y
(a:x) ++ y = a : (x ++ y)

null :: [a] -> Bool
null []    = True
null (_:_) = False

nil :: t -> [a]
nil    = const []
wrap :: a -> [a]
wrap   = cons . pair (id, nil)
cons :: (a, [a]) -> [a]
cons   = uncurry (:)
cat :: ([a], [a]) -> [a]
cat    = uncurry (++)
concat :: [[a]] -> [a]
concat = catalist ([], cat)
snoc :: ([a], a) -> [a]
snoc   = cat . cross (id, wrap)

head :: [a] -> a
head (a:_) = a
tail :: [a] -> [a]
tail (_:x) = x
split :: [a] -> (a, [a])
split      = pair (head, tail)

last :: [a] -> a
last = cata1list (id, outr)
init :: [a] -> [a]
init = cata1list (nil, cons)

inits :: [a] -> [[a]]
inits = catalist ([[]], extend)
  where extend (a, xs) = [[]] ++ list (a:) xs
tails :: [a] -> [[a]]
tails = catalist ([[]], extend)
  where extend (a, x:xs) = (a:x):x:xs
splits :: [a] -> [([a], [a])]
splits = zip . pair (inits, tails)

cpp :: ([a], [b]) -> [(a, b)]
cpp (x, y) = [(a, b) | a <- x, b <- y]
cpl :: ([a], b) -> [(a, b)]
cpl (x, b) = [(a, b) | a <- x]
cpr :: (a, [b]) -> [(a, b)]
cpr (a, y) = [(a, b) | b <- y]
cplist :: [[a]] -> [[a]]
cplist     = catalist ([[]], list cons . cpp)

minlist :: ((a, a) -> Bool) -> [a] -> a
minlist r = cata1list (id, bmin r)
bmin :: ((a, a) -> Bool) -> (a, a) -> a
bmin r    = cond r (outl, outr)

maxlist :: ((a, a) -> Bool) -> [a] -> a
maxlist r = cata1list (id, bmax r)
bmax :: ((a, a) -> Bool) -> (a, a) -> a
bmax r    = cond (r . swap) (outl, outr)

thinlist :: ((a, a) -> Bool) -> [a] -> [a]
thinlist r = catalist ([], bump r)
  where bump r (a, [])  = [a]
        bump r (a, b:x) | r (a, b)  = a:x
                        | r (b, a)  = b:x
                        | otherwise = a:b:x

length :: Num a => [t] -> a
length   = catalist (0, succ . outr)
sum :: Num a => [a] -> a
sum      = catalist (0, plus)
trans :: [[a]] -> [[a]]
trans    = cata1list (list wrap, list cons . zip)
list :: (a -> b) -> [a] -> [b]
list f   = catalist ([], cons . cross (f, id))
filter :: (a -> Bool) -> [a] -> [a]
filter p = catalist ([], cond (p . outl) (cons, outr))

catalist :: (b, (a, b) -> b) -> [a] -> b
catalist (c, f) []    = c
catalist (c, f) (a:x) = f (a, catalist (c, f) x)

cata1list :: (a -> b, (a, b) -> b) -> [a] -> b
cata1list (f, g) [a]   = f a
cata1list (f, g) (a:x) = g (a, cata1list (f, g) x)

cata2list :: ((a, a) -> b, (a, b) -> b) -> [a] -> b
cata2list (f, g) [a,b] = f (a, b)
cata2list (f, g) (a:x) = g (a, cata2list (f, g) x)

loop :: ((a, b) -> a) -> (a, [b]) -> a
loop f (a, [])  = a
loop f (a, b:x) = loop f (f (a, b), x)

merge :: ((a, a) -> Bool) -> ([a], [a]) -> [a]
merge _ ([], y)    = y
merge _ (x, [])    = x
merge r (a:x, b:y) | r (a, b)  = a : merge r (x, b:y)
                   | otherwise = b : merge r (a:x, y)

zip :: ([a], [b]) -> [(a, b)]
zip (x, [])    = []
zip ([], y)    = []
zip (a:x, b:y) = (a, b) : zip (x, y)

unzip :: [(a, b)] -> ([a], [b])
unzip = pair (list outl, list outr)

-- Word and line processing functions: ------------------------------
words :: String -> [String]
words = filter (not . null) . catalist ([[]], cond ok (glue, new))
  where ok (a, xs)     = (a /= ' ' && a /= '\n')
        glue (a, x:xs) = (a:x):xs
        new (a, xs)    = []:xs

lines :: String -> [String]
lines = catalist ([[]], cond ok (glue, new))
  where ok (a, xs)     = (a /= '\n')
        glue (a, x:xs) = (a:x):xs
        new (a,xs)     = []:xs

unwords :: [String] -> String
unwords = cata1list (id, join)
  where join (x, y) = x ++ " " ++ y

unlines :: [String] -> String
unlines = cata1list (id, join)
  where join (x, y) = x ++ "\n" ++ y

-- Essentials and built-in primitives: -------------------------------
primPrint :: Show a => a -> IO ()
primPrint = print

strict :: (a -> b) -> a -> b
strict = ($!)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

-- End of Algebra of Programming prelude ----------------------------
