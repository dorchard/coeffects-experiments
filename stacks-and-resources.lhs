> {-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

> import Semantics.Comonadic
> import Control.Comonad
> import Control.Comonad.Zip 

> import Data.Bits 

> import Debug.Trace

> data Res a = Res { val :: a, res :: Int } deriving Show

> instance Functor Res where
>     fmap f (Res x r) = Res (f x) r

> instance Comonad Res where
>     extract = val 
>     extend k (Res x r) = Res (k (Res x r)) r

> instance CZip Res where
>     czip _ (Res x r, Res y s) = (Res (x, y) (r + s))  -- combines inner and outer parts

> instance CUnzip Res where
>     cunzip (Res (x, y) r) = (Res x (r `div` 2), Res y (r `div` 2)) -- count on application

>
> comonadic [d| foo = \z -> (\x -> (+) (x 2) z) (\y -> (+) y res) |]

> comonadic [d| foo2 = \z -> (\f -> f 2) (\y -> res) |]

> comonadic [d| foo3 = \z -> (\y -> res) () |]

> comonadic [d| foo4 = \z -> res |]

> comonadic [d| foo5 = \z -> ((\x -> (\f -> x) ()) (\f -> res)) () |]

> comonadic [d| foo6 = \z -> ((\x -> x) (\f -> res)) () |]

-- can only get this answer out as a value..?


> data StackN a = StackN { value :: a, stackN :: Int } deriving Show

> instance Functor StackN where
>     fmap f (StackN x r) = StackN (f x) r

> instance Comonad StackN where
>     extract = value
>     extend k (StackN x r) = StackN (k (StackN x r)) r

> instance CZip StackN where
>     czip dbg (StackN x d, StackN y c) = -- (dbg ++ "\n call - " ++ show c ++ "\n def - " ++ show d ++ "\n") `trace`
>                                         StackN (x, y) (c + d) 

> instance CUnzip StackN where -- ("app - " ++ show r ++ "\n") `trace` 
>     cunzip (StackN (x, y) r) = (StackN x 1, StackN y r) 

                                 -- An important thing to realise here is that the left part of this tuple
                                 -- may or may NOT be a definition, it may actually be an access/computation of a function
                                 -- in which case the left part represents the application context just as much. 



Observationall equivalent but different programs


> comonadic [d| beta0 = \z -> stackN |]
> comonadic [d| beta1 = (\x -> (\z -> stackN)) () |]
> comonadic [d| eta = (\x -> (\y -> (\z -> stackN) y)) () |]


> tests2 = [("beta0", 1, beta0), ("beta1", 2, beta1), ("eta", 3, eta)]

> doTests2 = let init1 = StackN () 1
>                init2 = StackN 'a' 0
>                xs = map (\(n, val, f) -> (n, val, f init1 init2, (f init1 init2) == val)) tests2
>            in (foldl (\r (_, _, _, y) -> y && r) True xs, xs)

> comonadic [d| beta0a = (\z -> stackN) () |]
> comonadic [d| beta1b = ((\x -> (\z -> stackN)) ()) () |]
> comonadic [d| etac = ((\x -> (\y -> (\z -> stackN) y)) ()) () |]


> tests3 = [("beta0a", 1, beta0a), ("beta1b", 2, beta1b), ("etac", 3, etac)]

> doTests3 = let init1 = StackN () 19
>                xs = map (\(n, val, f) -> (n, val, f init1, (f init1) == val)) tests3
>            in (foldl (\r (_, _, _, y) -> y && r) True xs, xs)




> comonadic [d| sfoo0 = \z -> stackN |]
> comonadic [d| sfoo1 = \z -> (\y -> stackN) z |]
> comonadic [d| sfoo2 = \z -> (\y -> (\x -> stackN) y) z |]
> comonadic [d| sfoo3 = \z -> (\y -> (\x -> (\w -> stackN) x) y) z |]

> comonadic [d| sfoop = \z -> (\f -> (\x -> x ()) f) (\y -> stackN) |]
> comonadic [d| sfoopp = \z -> (\x -> x ()) (\y -> stackN) |]
> comonadic [d| sfooppp = \z -> (\y -> stackN) () |]
> comonadic [d| sfoopppp = \z -> stackN |]

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
> comonadic [d| nexp = \z -> (\f -> (f ()) ()) (\a -> (\b -> (\c -> (\x -> stackN)) b) a) |]
> comonadic [d| nexpA = \z -> (\f -> f ()) (\a -> (\b -> (\x -> stackN) b) a) |]
> comonadic [d| nexpC = \z -> ((\f -> (f ())) (\a -> (\b -> (\c -> (\x -> stackN)) b) a)) () |]

> comonadic [d| mexp = \z -> ((\x -> (\y -> x) ()) (\f -> stackN)) () |]
> comonadic [d| mexpp = \z -> ((\y -> (\f -> stackN)) ()) () |]
> comonadic [d| mexppp = \z -> (\f -> stackN) () |]

> comonadic [d| mexpH = \z -> ((\x -> (\y -> (\z -> x) ()) ()) (\f -> stackN)) () |]


> tests = [("sfoo0", 1, sfoo0),
>          ("sfoo1", 2, sfoo1),
>          ("sfoo2", 3, sfoo2),
>          ("sfoo3", 4, sfoo3), 
>          ("sfoop", 4, sfoop), 
>          ("sfoopp", 3, sfoopp),
>          ("sfooppp", 2, sfooppp),
>          ("sfoopppp", 1, sfoopppp),
>          ("sfoop2", 5, sfoop2),
>          ("sfoop3", 5, sfoop3),
>          ("sfoop4", 7, sfoop4),
>          ("sfoop4A", 8, sfoop4A),
>          ("sfoop4B", 8, sfoop4B),
>          ("sfoop5", 4, sfoop5),
>          ("sfoop5A", 5, sfoop5A), 
>          ("sfoop5B", 5, sfoop5B), 
>          ("sfoopm", 3, sfoopm), 
>          ("sfoopmA", 3, sfoopmA), 
>          ("nexp", 6, nexp), 
>          ("nexpA", 5, nexpA), 
>          ("nexpC", 6, nexpC),
>          ("mexp", 4, mexp),
>          ("mexpp", 3, mexpp),
>          ("mexppp", 2, mexppp),
>          ("mexpH", 5, mexpH)]

> testsV = [("sfoo0", 1, sfoo0),
>          ("sfoo1", 2, sfoo1),
>          ("sfoo2", 3, sfoo2),
>          ("sfoo3", 4, sfoo3), 
>          ("sfoop", 4, sfoop), 
>          ("sfoopp", 3, sfoopp),
>          ("sfooppp", 2, sfooppp),
>          ("sfoopppp", 1, sfoopppp),
>          ("sfoop2", 4, sfoop2),
>          ("sfoop3", 5, sfoop3),
>          ("sfoop4", 7, sfoop4),
>          ("sfoop4A", 8, sfoop4A),
>          ("sfoop4B", 8, sfoop4B),
>          ("sfoop5", 4, sfoop5),
>          ("sfoop5A", 5, sfoop5A), 
>          ("sfoop5B", 5, sfoop5B), 
>          ("sfoopm", 3, sfoopm), 
>          ("sfoopmA", 3, sfoopmA), 
>          ("nexp", 6, nexp), 
>          ("nexpA", 5, nexpA), 
>          ("nexpC", 6, nexpC),
>          ("mexp", 4, mexp),
>          ("mexpp", 3, mexpp),
>          ("mexppp", 2, mexppp)]

> doTests = let init1 = StackN () 1
>               init2 = StackN 'a' 0
>               xs = map (\(n, val, f) -> (n, val, f init1 init2, (f init1 init2) == val)) tests
>           in (foldl (\r (_, _, _, y) -> y && r) True xs, xs)

All true (NOT FOR mexp!)

 czip (StackN x d, StackN y c) = StackN (x, y) (d + c)
 cunzip (StackN (x, y) r) = (StackN x 1, StackN y r) 

Mostly false for

 czip (StackN x d, StackN y c) = StackN (x, y) (d + c)
 cunzip (StackN (x, y) r) = (StackN x (r+1), StackN y r) 

Mostly true (except for last one!)

 czip (StackN x d, StackN y c) = StackN (x, y) (if (d > c) then d else (d + c))
 cunzip (StackN (x, y) r) = (StackN x (r+1), StackN y r) 

Mostly false for

 czip (StackN x d, StackN y c) = StackN (x, y) (if (d > c) then d else (d + c))
 cunzip (StackN (x, y) r) = (StackN x 1, StackN y r) 



 (\f -> (f ()) ()) (\a -> (\b -> (\c -> (\x -> stackN)) b) a)
 ((\a -> (\b -> (\c -> (\x -> stackN)) b) a) ()) ()
 ((\b -> (\c -> (\x -> stackN)) b) ()) ()
 ((\c -> (\x -> stackN)) ()) ()
 ((\x -> stackN) ()
 stackN)



> comonadic [d| mexp = \z -> ((\x -> (\f -> x) ()) (\f -> stackN)) () |] -- Expected depth of 2 (some popping happens)



> comonadic [d| mexpa = \z -> (((\f -> (\f -> stackN)) ()) ()) |] -- Expected 3

> comonadic [d| oexp = \z -> ((\x -> x) (\f -> stackN)) () |]

> comonadic [d| mexp2 = \z -> (((\x -> (\f -> f)) ()) (\f -> stackN)) () |] -- Expected depth of 4

> init0 = StackN () 1
> init1 = StackN 'a' 0

> comonadic [d| betaA = \z -> (\x -> stackN) () |] 
> comonadic [d| betaB = \z -> stackN |]
> comonadic [d| etaA = \z -> (\x -> stackN) () |]
> comonadic [d| etaB = \z -> (\w -> (\x -> stackN) w) () |]

> comonadic [d| dexp = \z -> (\f -> f ()) ((\x -> (\y -> (\f -> stackN)) ()) ()) |]
> comonadic [d| dexpa = \z -> ((\x -> (\f -> stackN)) ()) () |]


> comonadic [d| big = \z -> (\a -> ((\f -> f ()) ((\x -> (\y -> (\f -> stackN)) ()) ()))) () |]

beta-redux and eta-exp respectively decrease and increase the stack depth

> tests = [("sfooO", 1, sfoo0),
>          ("sfoo1", 2, sfoo1),
>          ("sfoo2", 3, sfoo2),
>          ("sfoo3", 4, sfoo3),
>          ("sfoopppp", 1, sfoopppp),
>          ("sfooppp", 2, sfooppp),
>          ("sfoopp", 3, sfoopp),
>          ("sfoop", 4, sfoop),
>          ("sfoop2", 5, sfoop2),
>          ("sfoop3", 5, sfoop3),
>          ("sfoop4", 7, sfoop4),
>          ("sfoop4A", 8, sfoop4A),
>          ("sfoop4B", 8, sfoop4B),
>          ("sfoop5", 4, sfoop5),
>          ("sfoop5A", 5, sfoop5A),
>          ("sfoop5B", 5, sfoop5B),
>          ("sfoopm", 3, sfoopm),
>          ("sfoopmA", 3, sfoopmA),
>          ("nexp", 6, nexp),
>          ("mexp", 2, mexp),
>          ("mexpa", 2, mexp),
>          ("oexp", 2, oexp), -- could be 3 or 2?
>          ("dexp", 5, dexp),
>          ("dexpa", 3, dexpa),
>          ("betaA", 2, betaA),
>          ("betaB", 1, betaB),
>          ("etaA", 2, etaA),
>          ("etaB", 3, etaB)]

> doTests = let xs = map (\(n, val, f) -> (n, val, f init0 init1, val == (f init0 init1))) tests
>           in (foldl (\r (_, _, _, p) -> r && p) True xs, xs)
