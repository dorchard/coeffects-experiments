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


> data StackN a = StackN { value :: a, stackN :: Int } deriving Show

> instance Functor StackN where
>     fmap f (StackN x r) = StackN (f x) r

> instance Comonad StackN where
>     extract = value
>     extend k (StackN x r) = StackN (k (StackN x r)) r

> instance CZip StackN where
>     czip (StackN x d, StackN y c) = StackN (x, y) 
>                                      (if (d > c) then d else (d + c))

> instance CUnzip StackN where
>     cunzip (StackN (x, y) r) = if (r + 1) > 10 then error "StackN overflow"
>                               else (StackN x (r + 1), StackN y r) -- increment stackN counter


Observationall equivalent but different programs

> comonadic [d| sfoo0 = \z -> stackN |]
> comonadic [d| sfoo1 = \z -> (\y -> stackN) z |]
> comonadic [d| sfoo2 = \z -> (\y -> (\x -> stackN) y) z |]
> comonadic [d| sfoo3 = \z -> (\y -> (\x -> (\w -> stackN) x) y) z |]

*Main> sfoo0 (StackN () 0) (StackN 'a' 0)
0
*Main> sfoo1 (StackN () 0) (StackN 'a' 0)
1
*Main> sfoo2 (StackN () 0) (StackN 'a' 0)
2

> -- CBN = 3, CBV = 3
> comonadic [d| sfoopppp = \z -> stackN |]
> comonadic [d| sfooppp = \z -> (\y -> stackN) () |]
> comonadic [d| sfoopp = \z -> (\x -> x ()) (\y -> stackN) |]
> comonadic [d| sfoop = \z -> (\f -> (\x -> x ()) f) (\y -> stackN) |]

> -- CBN = 3, CBV = 
> comonadic [d| sfoop2 = \z -> (\f -> (\x -> x ()) f) ((\x -> (\y -> stackN)) ()) |]

> comonadic [d| sfoop3 = \z -> (\f -> f ()) (\x -> (\y -> (\z -> stackN) y) x) |]

> comonadic [d| sfoop4 = \z -> (\f -> (\s -> f ()) ()) (\x -> (\y -> (\z -> (\w -> stackN) z) y) x) |]

> comonadic [d| sfoop4A = \z -> (\f -> (\s -> f ()) ()) (\x -> (\y -> (\z -> (\w -> (\o -> stackN) w) z) y) x) |]

> comonadic [d| sfoop4B = \z -> (\f -> (\s -> (\t -> f ()) s) ()) (\x -> (\y -> (\z -> (\w -> stackN) z) y) x) |]


> comonadic [d| sfoop5 = \z -> (\f -> (\s -> f ()) ()) (\x -> stackN) |]

> comonadic [d| sfoop5A = \z -> (\f -> (\s -> (\w ->  f ()) ()) ()) (\x -> stackN) |]

> comonadic [d| sfoop5B = \z -> (\f -> (\s -> f ()) ()) (\x -> (\y -> stackN) x) |]


> comonadic [d| sfoopm = \z -> (\f -> (\x -> stackN) f) ((\x -> (\y -> stackN)) ()) |]

> comonadic [d| sfoopmA = \z -> (\f -> (\x -> stackN) f) (\x -> (\y -> (\z -> (\w -> stackN) z) y) x) |]

