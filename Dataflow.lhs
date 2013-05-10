> {-# LANGUAGE TemplateHaskell #-}
> 
> module Dataflow (dataflow, next, prev, constant, fby, czip, Stream) where
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

> prj 0 = [| fst |]
> prj n = [| $(prj (n-1)) . snd |]

> pair :: (a -> b) -> (a -> c) -> a -> (b, c)
> pair f g = \x -> (f x, g x)

> interp :: Exp -> [Name] -> Q Exp
> interp (LitE c) vars  = [| const $(return $ LitE c) |]
> interp (LamE [VarP n] e) vars = [| curry ($(interp e (vars ++ [n])) . czip) |]
> interp (AppE e1 e2) vars       = [| (uncurry ($(interp e1 vars))) . ((fmap fst) `pair` (fmap snd))
>                                     . (extend (extract `pair` $(interp e1 vars))) |]  

 interp (AppE (AppE (ConE n) e1) e2) = 
 interp (InfixE e1 e2 e3) = 

> interp (CondE e1 e2 e3) vars   = [| (\d -> if $(return e1) d then $(return e2) d else $(return e3) d) |]

> interp (LetE [ValD (VarP n) (NormalB e1) []] e2) vars = [| (extend $(interp e2 (vars ++ [n]))) . (extract `pair` $(interp e1 vars)) |]

> interp (VarE v) vars = case (nameBase v) of
>                           "fby" -> return $ VarE v
>                           "next" -> return $ VarE v
>                           "prev" -> return $ VarE v
>                           _      -> [| $(prj (fromJust $ elemIndex v vars)) . extract |]

                           "+" ->
                           "*" ->
                           "-" ->
                           "/" -> 
                           
> interp _ _ = error "Syntax not allowed"