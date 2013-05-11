> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Dataflow

> x = [dataflow| let x = 1 in (fby x 2) + 3 |]

Run by 'extend x context'