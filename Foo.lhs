> {-# LANGUAGE QuasiQuotes #-}

> import Dataflow

> x = [dataflow| let x = 1 in (fby x 2) + 3 |]