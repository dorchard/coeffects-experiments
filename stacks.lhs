> {-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

> import Semantics.Comonadic
> import Control.Comonad
> import Control.Comonad.Zip 

> import Debug.Trace

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



> comonadic [d| sfoo0 = \z -> stackN |]
> comonadic [d| sfoo1 = \z -> (\y -> stackN) z |]
> comonadic [d| sfoo2 = \z -> (\y -> (\x -> stackN) y) z |]
> comonadic [d| sfoo3 = \z -> (\y -> (\x -> (\w -> stackN) x) y) z |]

> comonadic [d| sfoopppp = \z -> stackN |]
> comonadic [d| sfooppp = \z -> (\y -> stackN) () |]
> comonadic [d| sfoopp = \z -> (\x -> x ()) (\y -> stackN) |]
> comonadic [d| sfoop = \z -> (\f -> (\x -> x ()) f) (\y -> stackN) |]

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
> comonadic [d| dexp = \z -> (\f -> f ()) ((\x -> (\y -> (\f -> stackN)) ()) ()) |]
> comonadic [d| dexpa = \z -> ((\x -> (\f -> stackN)) ()) () |]

> init0 = StackN () 1
> init1 = StackN 'a' 0

> comonadic [d| betaA = \z -> (\x -> stackN) () |] 
> comonadic [d| betaB = \z -> stackN |]
> comonadic [d| etaA = \z -> (\x -> stackN) () |]
> comonadic [d| etaB = \z -> (\w -> (\x -> stackN) w) () |]

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
>          ("oexp", 2, oexp),
>          ("dexp", 5, dexp),
>          ("dexpa", 3, dexpa),
>          ("betaA", 2, betaA),
>          ("betaB", 1, betaB),
>          ("etaA", 2, etaA),
>          ("etaB", 3, etaB)]

> doTests = let xs = map (\(n, val, f) -> (n, val, f init0 init1, val == (f init0 init1))) tests
>           in (foldl (\r (_, _, _, p) -> r && p) True xs, xs)