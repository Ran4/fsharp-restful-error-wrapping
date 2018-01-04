module Rop

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure
    
    
let (>>=) prevResult f =
    match prevResult with
    | Success value -> f value
    | Failure value -> Failure value
    
let (>>!) prevResult onErrorFunc =
    match prevResult with
    | Success value -> Success value
    | Failure value -> onErrorFunc value
    
let bind f prevResult =
    prevResult >>= f

let onError onErrorFunc prevResult =
    prevResult >>! onErrorFunc
    
let switch f =
    f >> Success
    
let fail f =
    f >> Failure
    
let either successFunc failureFunc prevResult =
    match prevResult with
    | Success value -> successFunc value
    | Failure value -> failureFunc value
    
let tee f x =
    f x |> ignore
    x
