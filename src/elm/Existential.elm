module Existential exposing (add, impl, init, map, wrap)


impl : t -> (raise -> rep -> t)
impl constructor =
    \raise rep -> constructor


wrap : (raise -> rep -> t) -> (raise -> rep -> (t -> q)) -> (raise -> rep -> q)
wrap method pipeline raise rep =
    method raise rep |> pipeline raise rep


add : (rep -> t) -> (raise -> rep -> (t -> q)) -> (raise -> rep -> q)
add method pipeline raise rep =
    method rep |> pipeline raise rep


map : (a -> b) -> (raise -> rep -> a) -> (raise -> rep -> b)
map op pipeline raise rep =
    pipeline raise rep |> op


init : ((rep -> sealed) -> flags -> output) -> ((rep -> sealed) -> rep -> sealed) -> flags -> output
init initRep pipeline flags =
    let
        raise : rep -> sealed
        raise rep =
            pipeline raise rep
    in
    initRep raise flags
