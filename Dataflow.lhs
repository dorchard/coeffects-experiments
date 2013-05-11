> {-# LANGUAGE TemplateHaskell #-}
> 
> module Dataflow ({- export top-level macro -}      dataflow,                  
>                  {- export Lucid primitives -}     next, prev, constant, fby, 
>                  {- export semantics primitives -} czip, Stream, extract, extend, context) where
>
> import Lucid

> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Quote
> import Language.Haskell.Meta.Parse

> import Control.Comonad

> import Data.List
> import Data.Maybe

> import Debug.Trace
> 
> dataflow :: QuasiQuoter 
> dataflow = QuasiQuoter { quoteExp = interpretDataflow,
>                          -- Just need to understand expressions
>                          quotePat = undefined,  
>                          quoteType = undefined,
>                          quoteDec = undefined }

> interpretDataflow s = case parseExp s of
>                          Left l -> error l
>                          Right e -> interp e [] 

> pair :: (a -> b) -> (a -> c) -> a -> (b, c)
> pair f g = \x -> (f x, g x)

Interprets a Haskell expression as a comonadic Lucid expression given
           - Exp (Haskell exp)
           - [Name] (list of locally bound variables)

> interp :: Exp -> [Name] -> Q Exp
> interp (LitE c) vars          = [| const $(return $ LitE c) |]
> interp (LamE [VarP n] e) vars = [| curry ($(interp e (vars ++ [n])) . czip) |]
> interp (AppE e1 e2) vars      = [| (uncurry ($(interp e1 vars))) . ((fmap fst) `pair` (fmap snd))
>                                     . (extend (extract `pair` $(interp e2 vars))) |]  

> interp (UInfixE e1 e e2) vars = interp (AppE (AppE e e1) e2) vars

> interp (CondE e1 e2 e3) vars  = [| (\d -> if $(return e1) d then $(return e2) d else $(return e3) d) |]

> interp (LetE [ValD (VarP n) (NormalB e1) []] e2) vars 
>                               = [| ($(interp e2 (vars ++ [n]))) . (extend (extract `pair` $(interp e1 vars))) |]

> interp (VarE v) vars          = case (nameBase v) of
>                                 "fby" -> [| const $(return $ VarE v) |]
>                                 "next" -> [| const $(return $ VarE v) |]
>                                 "prev" -> [| const $(return $ VarE v) |]
>                                 "+"    -> [| const (\x -> \y -> (extract x + extract y)) |]
>                                 _      -> [| $(prj ((length vars) - (fromJust $ elemIndex v vars) - 1)) . extract |]

> interp (ParensE e) vars       = [| ($(interp e vars)) |]
> interp t _                    = error $ "Syntax not allowed: " ++ show t

Computers a projection from an environment (for semantics of variables)

> prj 0 = [| snd |]
> prj n = [| $(prj (n-1)) . fst |]