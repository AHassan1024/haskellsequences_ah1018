module Sequences where

import Data.Char (ord, chr)

maxOf2 :: Int -> Int -> Int
-- Returns first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 x1 x2
    | x1 >= x2   = x1
    | otherwise = x2

maxOf3 :: Int -> Int -> Int -> Int
-- Returns the largest of three Ints
maxOf3 x y z
    = maxOf2 (maxOf2 x y) z


isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit c
    = c `elem` ['0'..'9']

isAlpha :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha c
    = c `elem` ['a'..'z'] 
      || c `elem` ['A'..'Z']

digitToInt :: Char -> Int
-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
-- Precondiditon: the character is one of '0'..'9'
digitToInt c
    = ord c - ord '0'

toUpper :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper c
    | c `elem` ['a'..'z'] = chr(ord c - (ord 'a' - ord 'A'))
    | otherwise           = error "You have not entered a character."


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
