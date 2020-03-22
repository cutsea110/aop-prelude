{-# LANGUAGE NoImplicitPrelude #-}
module AOPPrelude where
---------------------------------------------------------------------
-- Prelude for `Algebra of Programming' -----------------------------
-- Original created 14 Sept, 1995, by Richard Bird ------------------
---------------------------------------------------------------------

-- Operator precedence table: ---------------------------------------
import GHC.Base ((==), (/=), (<), (<=), (>=), (>))
import GHC.Err (error)
import GHC.Num ((+), (-), (*), negate)
import GHC.Real ((/), div, mod, Fractional)
import GHC.Show (Show, show)
import GHC.Classes hiding (not, (&&), (||))
import GHC.Types

import Data.Char (ord, chr)
import System.IO (print)

infixr 9 .
infixr 5 ++
infixr 3 &&
infixr 2 ||

-- Standard combinators: --------------------------------------------

(f . g) x = f (g x)
const k a = k
id a      = a

outl (a, _) = a
outr (_, b) = b
swap (a, b) = (b, a)

assocl (a, (b, c)) = ((a, b), c)
assocr ((a, b), c) = (a, (b, c))

dupl (a, (b, c)) = ((a, b), (a, c))
dupr ((a, b), c) = ((a, c), (b, c))

pair (f, g) a       = (f a, g a)
cross (f, g) (a, b) = (f a, g b)
cond p (f, g) a     = if p a then f a else g a

curry f a b      = f (a, b)
uncurry f (a, b) = f a b

-- Boolean functions: -----------------------------------------------

false = const False
true  = const True

False && _ = False
True  && x = x

False || x = x
True  || _ = True

not True   = False
not False  = True

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

meet (r, s) = cond r (s, false)
join (r, s) = cond r (true, s)
wok r       = r . swap

-- Numerical functions: ---------------------------------------------

zero   = const 0
succ   = (+1)
pred   = (-1)
plus   = uncurry (+)
minus  = uncurry (-)
times  = uncurry (*)
divide :: Fractional a => (a, a) -> a
divide = uncurry (/)

negative = (< 0)
positive = (> 0)

-- List-processing functions: ---------------------------------------

[] ++ y    = y
(a:x) ++ y = a : (x ++ y)

null []    = True
null (_:_) = False

nil    = const []
wrap   = cons . pair (id, nil)
cons   = uncurry (:)
cat    = uncurry (++)
concat = catalist ([], cat)
snoc   = cat . cross (id, wrap)

head (a:_) = a
tail (_:x) = x
split      = pair (head, tail)

last = cata1list (id, outr)
init = cata1list (nil, cons)

inits = catalist ([[]], extend)
  where extend (a, xs) = [[]] ++ list (a:) xs

tails = catalist ([[]], extend)
  where extend (a, x:xs) = (a:x):x:xs
splits = zip . pair (inits, tails)

cpp (x, y) = [(a, b) | a <- x, b <- y]
cpl (x, b) = [(a, b) | a <- x]
cpr (a, y) = [(a, b) | b <- y]
cplist     = catalist ([[]], list cons . cpp)

minlist r = cata1list (id, bmin r)
bmin r    = cond r (outl, outr)

maxlist r = cata1list (id, bmax r)
bmax r    = cond (r . swap) (outl, outr)

thinlist r = catalist ([], bump r)
  where bump r (a, [])  = [a]
        bump r (a, b:x) | r (a, b)  = a:x
                        | r (b, a)  = b:x
                        | otherwise = a:b:x

length   = catalist (0, succ . outr)
sum      = catalist (0, plus)
trans    = cata1list (list wrap, list cons . zip)
list f   = catalist ([], cons . cross (f, id))
filter p = catalist ([], cond (p . outl) (cons, outr))


catalist (c, f) []    = c
catalist (c, f) (a:x) = f (a, catalist (c, f) x)

cata1list (f, g) [a]   = f a
cata1list (f, g) (a:x) = g (a, cata1list (f, g) x)

cata2list (f, g) [a,b] = f (a, b)
cata2list (f, g) (a:x) = g (a, cata2list (f, g) x)

loop f (a, [])  = a
loop f (a, b:x) = loop f (f (a, b), x)

merge _ ([], y)    = y
merge _ (x, [])    = x
merge r (a:x, b:y) | r (a, b)  = a : merge r (x, b:y)
                   | otherwise = b : merge r (a:x, y)

zip (x, [])    = []
zip ([], y)    = []
zip (a:x, b:y) = (a, b) : zip (x, y)

unzip = pair (list outl, list outr)

-- Word and line processing functions: ------------------------------

words = filter (not . null) . catalist ([[]], cond ok (glue, new))
  where ok (a, xs)     = (a /= ' ' && a /= '\n')
        glue (a, x:xs) = (a:x):xs
        new (a, xs)    = []:xs

lines = catalist ([[]], cond ok (glue, new))
  where ok (a, xs)     = (a /= '\n')
        glue (a, x:xs) = (a:x):xs
        new (a,xs)     = []:xs

unwords = cata1list (id, join)
  where join (x, y) = x ++ " " ++ y

unlines = cata1list (id, join)
  where join (x, y) = x ++ "\n" ++ y

-- Essential and built-in primitives: -------------------------------

primPrint :: Show a => a -> IO ()
primPrint = print
-- strict = undefined -- FIXME!

flip f a b = f b a

-- End of Algebra of Programming prelude ----------------------------
