> module Lucid where

> import Control.Comonad

> data Stream a = Stream (Int -> a) Int

> instance Comonad Stream where
>     extract (Stream s c) = s c
>     extend k (Stream s c) = Stream (\c' -> k (Stream s c')) c

> instance Functor Stream where
>     fmap f (Stream s c) = Stream (\c -> f (s c)) c

> next :: Stream a -> a
> next (Stream s c) = s (c + 1)

> prev :: Stream a -> a
> prev (Stream s c) = s (c - 1)

> constant :: a -> Stream a
> constant x = Stream (\_ -> x) 0

> fby :: Stream a -> Stream a -> a
> fby ~(Stream s c) ~(Stream t d) = if (c==0 && d==0) then s 0 else t (d-1)

 fby :: Stream (a, b) -> a
 fby (Stream s c) = if (c == 0) then fst $ s 0 else snd $ s (c - 1)

> czip :: (Stream a, Stream b) -> Stream (a, b)
> czip ~(Stream s c, Stream t d) = Stream (\c' -> (s (c' - (f c d) + c), t (c' - (f c d) + d))) (f c d)
>                                  where f a b = min a b
>     
