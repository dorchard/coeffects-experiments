> {-# LANGUAGE TemplateHaskell #-}
> 
> module Semantics.Comonadic where
>
> import Control.Comonad.Zip

> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Control.Comonad

> import Data.List
> import Data.Maybe

> import Debug.Trace

 comonadic e = e >>= (\e' -> interp e' [])

> pair :: (a -> b) -> (a -> c) -> a -> (b, c)
> pair f g = \x -> (f x, g x)

Interprets a Haskell expression as a comonadic Lucid expression given
           - Exp (Haskell exp)
           - [Name] (list of locally bound variables)

> comonadic :: Q [Dec] -> Q [Dec]
> comonadic qdecs = qdecs >>= mapM interpDec

> class (Comonad c, CZip c, CUnzip c) => ComonadLam c where
>     

> interpDec :: Dec -> Q Dec 
> interpDec (ValD pat (NormalB e) decs) = (interp e) >>= (\e' -> return $ ValD pat (NormalB e') decs)
> interpDec (FunD n clauses) = (mapM interpClause clauses) >>= (\clauses' -> return (FunD n clauses'))
> interpDec d =  return d

> interpClause (Clause pats (NormalB e) decs) = (interp e) >>= (\e' -> return $ Clause pats (NormalB e') decs)

> interp :: Exp -> Q Exp
> interp e = interp' e []

> interp' :: Exp -> [Name] -> Q Exp
> interp' (LitE c) vars          = [| const $(return $ LitE c) |]
> interp' (LamE [VarP n] e) vars = [| curry ($(interp' e (vars ++ [n])) . czip) |]
> interp' (AppE e1 e2) vars      = [| (uncurry ($(interp' e1 vars))) . cunzip . 
>                                      (extend (extract `pair` $(interp' e2 vars))) |]  

> interp' (UInfixE e1 e e2) vars = interp' (AppE (AppE e e1) e2) vars

> interp' (CondE e1 e2 e3) vars  = [| (\d -> if $(return e1) d then $(return e2) d else $(return e3) d) |]

> interp' (LetE [ValD (VarP n) (NormalB e1) []] e2) vars 
>                               = [| ($(interp' e2 (vars ++ [n]))) . (extend (extract `pair` $(interp' e1 vars))) |]

> interp' (VarE v) vars          = case (nameBase v) of
>                                 --"fby" -> [| const $(return $ VarE v) |]
>                                 --"next" -> [| const $(return $ VarE v) |]
>                                 --"res" -> VarE $ mkName "res" 
>                                 "+"    -> [| const (\x -> \y -> (extract x + extract y)) |]
>                                 --"+"    -> [| const (\x -> \y -> (extract x + extract y)) |]
>                                 _      -> if (elem v vars) then
>                                             [| $(prj ((length vars) - (fromJust $ elemIndex v vars) - 1)) . extract |]
>                                           else return $ VarE v

> interp' (ParensE e) vars       = [| ($(interp' e vars)) |]
> interp' (ConE name) vars  | nameBase name == "()" 
>                                = [| const () |]
> interp' t _                    = error $ "Syntax not allowed: " ++ show t

Computers a projection from an environment (for semantics of variables)

> prj 0 = [| snd |]
> prj n = [| $(prj (n-1)) . fst |]