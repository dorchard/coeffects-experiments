{-# LANGUAGE DataKinds, TypeOperators, KindSignatures #-}

module BLL where

import GHC.TypeLits

type a ? (n :: Nat) = a