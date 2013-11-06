> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Data.Set
> import ImplicitParam

> x = [infer| let x = ?q + 3 in  ?p + ?q |]
