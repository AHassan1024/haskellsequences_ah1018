module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
-- Returns first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 x1 x2
    | x1 >= x2   = x1
    | otherwise = x2

maxOf3 :: Int -> Int -> Int -> Int
maxOf3 x y z
    = maxOf2 (maxOf2 x y) z
-- Returns the largest of three Ints

isADigit :: Char -> Bool
isADigit c
    = c `elem` ['0'..'9']
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise

-- False otherwise
isAlpha :: Char -> Bool
isAlpha c
    = c `elem` ['a'..'z'] 
      || c `elem` ['A'..'Z']
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';

digitToInt :: Char -> Int
digitToInt c
-- Precondiditon: the character is one of '0'..'9'
    = ord c - ord '0'
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.

toUpper :: Char -> Char
toUpper c
    | c `elem` ['a'..'z'] = chr(ord c - (ord 'a' - ord 'A'))
    | otherwise           = error "You have not entered a character."
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n
    | n <= 1    = a
    | otherwise = arithmeticSeq (a + d) d (n - 1)

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n
    | n <= 1    = a
    | otherwise = a * geometricSeq (a * r) r (n - 1)

-- Arithmetic series (the sum of terms of a sequence; to its nth term)
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries _ _ 0 = 0.0
arithmeticSeries a d n
    = a + arithmeticSeries (a + d) d (n - 1) 

-- Geometric series (the sum of terms of a sequence, to its nth term.)
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
    = a * (1 - (r ^ n)) / (1 - r)
