> module Control.Comonad.Zip where

> class CZip c where
>     czip :: (c a, c b) -> c (a, b)

> class CUnzip c where
>     cunzip :: c (a, b) -> (c a, c b)