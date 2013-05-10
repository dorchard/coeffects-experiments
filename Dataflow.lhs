> {-# LANGUAGE TemplateHaskell #-}
> 
> module Dataflow (dataflow, next, prev, constant, fby, czip, Stream, extract, extend) where
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

> interpretDataflow s = do --loc <- location
>                          --let pos = (loc_filename loc,fst (loc_start loc), snd (loc_start loc))
>                          case parseExp s of
>                            Left l -> error l
>                            Right e -> interp e []

> prj 0 = [| snd |]
> prj n = [| $(prj (n-1)) . fst |]

> pair :: (a -> b) -> (a -> c) -> a -> (b, c)
> pair f g = \x -> (f x, g x)

> interp :: Exp -> [Name] -> Q Exp
> interp (LitE c) vars  = [| const $(return $ LitE c) |]
> interp (LamE [VarP n] e) vars = [| curry ($(interp e (vars ++ [n])) . czip) |]
> interp (AppE e1 e2) vars       = [| (uncurry ($(interp e1 vars))) . ((fmap fst) `pair` (fmap snd))
>                                     . (extend (extract `pair` $(interp e2 vars))) |]  

 interp (AppE (AppE (ConE n) e1) e2) = 
 interp (InfixE e1 e2 e3) = 

> interp (CondE e1 e2 e3) vars   = [| (\d -> if $(return e1) d then $(return e2) d else $(return e3) d) |]

> interp (LetE [ValD (VarP n) (NormalB e1) []] e2) vars = [| ($(interp e2 (vars ++ [n]))) . (extend (extract `pair` $(interp e1 vars))) |]

> interp (VarE v) vars = case (nameBase v) of
>                           "fby" -> [| const $(return $ VarE v) |]
>                           "next" -> [| const $(return $ VarE v) |]
>                           "prev" -> [| const $(return $ VarE v) |]
>                           _      -> [| $(prj ((length vars) - (fromJust $ elemIndex v vars) - 1)) . extract |]

                           "+" ->
                           "*" ->
                           "-" ->
                           "/" -> 
                           
> interp _ _ = error "Syntax not allowed"