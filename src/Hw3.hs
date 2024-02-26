-- | CSE 116: All about fold.
--
--     For this assignment, you may use the following library functions:
--
--     length
--     append (++)
--     map
--     foldl'
--     foldr
--     unzip
--     zip
--     reverse
--
--  Use www.haskell.org/hoogle to learn more about the above.
--
--  Do not change the skeleton code! The point of this assignment
--  is to figure out how the functions can be written this way
--  (using fold). You may only replace the `error "TBD:..."` terms.
module Hw3 where

import Data.List (foldl')
import Prelude hiding (replicate, sum)

foldLeft :: (a -> b -> a) -> a -> [b] -> a
foldLeft = foldl'

--------------------------------------------------------------------------------

-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30
sqSum :: [Int] -> Int
sqSum xs = foldLeft f base xs
  where
    f a x = a + x * x
    base = 0

--------------------------------------------------------------------------------

-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24
pipe :: [(a -> a)] -> (a -> a)
pipe fs = foldLeft f base fs
  where
    f a x = a . x
    base = id

--------------------------------------------------------------------------------

-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"
sepConcat :: String -> [String] -> String
sepConcat sep [] = ""
sepConcat sep (x : xs) = foldLeft f base l
  where
    f a x = a ++ sep ++ x
    base = x
    l = xs

intString :: Int -> String
intString = show

--------------------------------------------------------------------------------

-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1, 2, 3, 4, 5, 6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1, 2, 3], [4, 5], [6], []]"
stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = "[" ++ sepConcat ", " (map f xs) ++ "]"

--------------------------------------------------------------------------------

-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]
--
-- >>> clone "foo" 2
-- ["foo", "foo"]
clone :: a -> Int -> [a]
clone _ 0 = []
clone x n = [x] ++ clone x (n - 1)

type BigInt = [Int]

--------------------------------------------------------------------------------

-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- ([0,0,9,9],[1,0,0,2])
--
-- >>> padZero [1,0,0,2] [9,9]
-- ([1,0,0,2], [0,0,9,9])
padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2 =
  let diff = abs (length l1 - length l2)
   in if length l1 > length l2
        then (l1, clone 0 diff ++ l2)
        else (clone 0 diff ++ l1, l2)

--------------------------------------------------------------------------------

-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]
--
-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []
removeZero :: BigInt -> BigInt
removeZero ds = foldLeft f base ds
  where
    f a x = if null a && x == 0 then [] else a ++ [x]
    base = []

--------------------------------------------------------------------------------

-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1, 1, 0, 1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1, 0, 9, 9, 8]
bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2 = removeZero (removeCarry res)
  where
    (l1', l2') = padZero l1 l2
    res = foldLeft f base args
    f a x = sum a x
      where
        sum (carry, array) (l1'', l2'') = (newCarry, newNum : array) -- return type (Int, [Int])
          where
            tempSum = l1'' + l2'' + carry
            newCarry = tempSum `div` 10
            newNum = tempSum `mod` 10
    removeCarry (carry, array) = newArray
      where
        newArray = if carry > 0 then carry : array else array
    base = (0, [])
    args = reverse (zip l1' l2')

--------------------------------------------------------------------------------

-- | `mulByInt i n` returns the result of multiplying
--   the int `i` with `BigInt` `n`.
--
-- >>> mulByInt 9 [9,9,9,9]
-- [8,9,9,9,1]
mulByInt :: Int -> BigInt -> BigInt
mulByInt i n = foldLeft f base arg
  where
    arg = clone n i
    base = [0]
    f a x = bigAdd a x

--------------------------------------------------------------------------------

-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]
bigMul :: BigInt -> BigInt -> BigInt
bigMul l1 l2 = res
  where
    (_, res) = foldLeft f base args
    f a x = step a x
      where
        step (index, sum) x = (index + 1, bigAdd currentSum sum)
          where
            currentSum = mulByInt x' l2
              where
                x' = if index > 0 then (x * (10 ^ index)) else x
    base = (0, [])
    args = reverse l1
