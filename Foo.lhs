> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}

> import Dataflow
> import Coeffect

> x = [dataflow| let x = 1 in (fby x 2) + 3 |]



Run by 'extend x context'

> y = [coeffect| let x = 1 in x |]