{-# LANGUAGE DataKinds, TypeOperators #-}

module Foo where

import BLL

f :: Int ? 2 -> Int
f x = x + x 

g :: Int ? 2 -> Int ? 1 -> Maybe Int -> Int
g x y (Just z) = x + x + y

h :: Int ? 3 -> Int
h x = x + y where y = x + x

foo :: (a ? n -> b) f

-- TODO - deal with this
--a :: Int ? 2 -> Int
--a x = x + let x = 2 in x 