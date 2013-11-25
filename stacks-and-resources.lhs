> {-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

> import Semantics.Comonadic
> import Control.Comonad
> import Control.Comonad.Zip

> data Res a = Res { val :: a, res :: Int } deriving Show

> instance Functor Res where
>     fmap f (Res x r) = Res (f x) r

> instance Comonad Res where
>     extract = val 
>     extend k (Res x r) = Res (k (Res x r)) r

> instance CZip Res where
>     czip (Res x r, Res y s) = (Res (x, y) (r + s))  -- combines inner and outer parts

> instance CUnzip Res where
>     cunzip (Res (x, y) r) = (Res x 0, Res y (r+1)) -- count on application

>
> comonadic [d| foo = \z -> (\x -> (+) (x 2) z) (\y -> (+) y res) |]

> comonadic [d| foo2 = \z -> (\f -> f 2) (\y -> res) |]

> comonadic [d| foo3 = \z -> (\y -> res) 2 |]

> comonadic [d| foo4 = \z -> res |]

-- can only get this answer out as a value..?


> data Stack a = Stack { value :: a, stack :: Int } deriving Show

> instance Functor Stack where
>     fmap f (Stack x r) = Stack (f x) r

> instance Comonad Stack where
>     extract = value
>     extend k (Stack x r) = Stack (k (Stack x r)) r

> instance CZip Stack where
>     czip (Stack x r, Stack y s) = Stack (x, y) r -- only care about stack from outer-environment
>                                                  -- not stack usage from inner

if (r + s + 1 > 2) then error "Stack overflow!"
                               else (Stack (x, y) (r + s + 1))  -- increment stack counter

> instance CUnzip Stack where
>     cunzip (Stack (x, y) r) = (Stack x (r + 1), Stack y r) -- increment stack counter


Observationall equivalent but different programs

> comonadic [d| sfoo0 = \z -> stack |]
> comonadic [d| sfoo1 = \z -> (\y -> stack) z |]
> comonadic [d| sfoo2 = \z -> (\y -> (\z -> stack) y) z |]

