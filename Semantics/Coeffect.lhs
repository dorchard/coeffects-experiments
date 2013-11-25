> {-# LANGUAGE TemplateHaskell, TypeFamilies, KindSignatures, ConstraintKinds,
>    MultiParamTypeClasses #-}
> 
> module Semantics.Coeffect 
>                 ({- export top-level macro -}      coeffect,                  
>                  {- export semantics primitives -} izip, iunzip, iextract, iextend) where

> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse

> import Data.List
> import Data.Maybe

> import Debug.Trace
> 
> import GHC.Prim

> class IxComonad (c :: * -> * -> *) where
>     type Inv c s t :: Constraint -- invariants (i.e. restrict to a subcategory of Hask)
>     type Inv c s t = ()

>     data Unit c 
>     type Plus c s t

>     type Plus c (Unit c) t = t
>     type Plus c t (Unit c) = t

>
>     iextract :: c (Unit c) a -> a
>     iextend :: Inv c s t => (c t a -> b) -> c (Plus c s t) a -> c s b
>     icmap :: Inv c s s => (a -> b) -> c s a -> c s b

> class IxZip (c :: * -> * -> *) where
>     type Meet c s t
>     izip :: c s a -> c t b -> c (Meet c s t) (a, b)

> class IxUnzip (c :: * -> * -> *) s t where
>     type Join c s t
>     iunzip :: c (Join c s t) (a, b) -> (c s a, c t b)

> coeffect :: QuasiQuoter 
> coeffect = QuasiQuoter { quoteExp = interpretCoeffects,
>                          -- Just need to understand expressions
>                          quotePat = undefined,  
>                          quoteType = undefined,
>                          quoteDec = undefined }

> interpretCoeffects s = case parseExp s of
>                          Left l -> error l
>                          Right e -> interp e [] 

> pair :: (a -> b) -> (a -> c) -> a -> (b, c)
> pair f g = \x -> (f x, g x)

> (><) :: (a -> b) -> (x -> y) -> (a, x) -> (b, y)
> f >< g = \(x, y) -> (f x, g y)

> delta :: a -> (a, a)
> delta x = (x, x)

Interprets a Haskell expression as a comonadic Lucid expression given
           - Exp (Haskell exp)
           - [Name] (list of locally bound variables)

> interp :: Exp -> [Name] -> Q Exp
> interp (LitE c) vars          = [| (const $(return $ LitE c)) . iextract |]
> interp (LamE [VarP n] e) vars = [| curry ($(interp e (vars ++ [n])) . izip) |]
> interp (AppE e1 e2) vars      = [| (uncurry ($(interp e1 vars))) . iunzip . 
>                                      (iextend ((iextract >< $(interp e2 vars)) . 
>                                         iunzip . icmap delta)) |]  

> interp (UInfixE e1 e e2) vars = interp (AppE (AppE e e1) e2) vars

 interp (CondE e1 e2 e3) vars  = [| (\d -> if $(return e1) d then $(return e2) d else $(return e3) d) |]

> interp (LetE [ValD (VarP n) (NormalB e1) []] e2) vars 
>                               = [| ($(interp e2 (vars ++ [n]))) . (iextend ((iextract >< $(interp e1 vars)) . iunzip . icmap delta)) |]

> interp (VarE v) vars          = case (nameBase v) of
>                                 "+"    -> [| const (\x -> \y -> (iextract x + iextract y)) |]
>                                 _      -> [| $(prj ((length vars) - (fromJust $ elemIndex v vars) - 1)) . iextract |]

> interp (ParensE e) vars       = [| ($(interp e vars)) |]
> interp t _                    = error $ "Syntax not allowed: " ++ show t

Computers a projection from an environment (for semantics of variables)

> prj 0 = [| snd |]
> prj n = [| $(prj (n-1)) . fst |]