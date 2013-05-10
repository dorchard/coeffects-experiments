> {-# LANGUAGE QuasiQuotes #-}

> import Dataflow

> x = [dataflow| let x = 1 in next x |]