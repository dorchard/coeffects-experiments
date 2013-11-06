> {-# LANGUAGE TemplateHaskell, TypeFamilies, KindSignatures, ConstraintKinds,
>    MultiParamTypeClasses #-}
> 
> module ImplicitParam  where

> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse

> import Data.Maybe
> import Data.Set

> import Debug.Trace
> 
> import GHC.Prim


> infer :: QuasiQuoter 
> infer = QuasiQuoter { quoteExp = inferCoeffects,
>                       -- Just need to understand expressions
>                       quotePat = undefined,  
>                       quoteType = undefined,
>                       quoteDec = undefined }

> inferCoeffects s = case parseExp (sneekQMark s) of
>                          Left l -> error l
>                          Right e -> show (interp e []) `trace` [| () |]

> sneekQMark [] = []
> sneekQMark ('?':xs) = 'p':(sneekQMark xs)
> sneekQMark (x:xs) = x:(sneekQMark xs)

> pair :: (a -> b) -> (a -> c) -> a -> (b, c)
> pair f g = \x -> (f x, g x)

> (><) :: (a -> b) -> (x -> y) -> (a, x) -> (b, y)
> f >< g = \(x, y) -> (f x, g y)

> delta :: a -> (a, a)
> delta x = (x, x)

Interprets a Haskell expression as a comonadic Lucid expression given
           - Exp (Haskell exp)
           - [Name] (list of locally bound variables)

> interp :: Exp -> [Name] -> Set (String, ())
> interp (LitE c) vars          =  empty 
> interp (VarE v) vars          =  case (nameBase v) of
>                                    ('p':var) -> insert ('?':var, ()) empty --UnboxedTupleT 0
>                                    _         -> empty

 interp (LamE [VarP n] e) vars = [|  |]

> interp (AppE e1 e2) vars      = (interp e1 vars) `union` (interp e2 vars)

> interp (UInfixE e1 e e2) vars = interp (AppE (AppE e e1) e2) vars

 interp (CondE e1 e2 e3) vars  = [| (\d -> if $(return e1) d then $(return e2) d else $(return e3) d) |]

> interp (LetE [ValD (VarP n) (NormalB e1) []] e2) vars 
>                               = (interp e1 vars) `union` (interp e2 vars)


> interp (ParensE e) vars       = interp e vars
> interp t _                    = error $ "Syntax not allowed: " ++ show t

Computers a projection from an environment (for semantics of variables)


 prj 0 = [| snd |]
 prj n = [| $(prj (n-1)) . fst |]