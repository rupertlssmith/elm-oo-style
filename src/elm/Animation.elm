module Animation exposing (Animation, animate, empty, step, subscriptions)

{-| An animation interpolates properties of a model based on a progress value that runs from
0.0 to 1.0, in a given length of time.

An easing function can be applied to this. An easing function should map 0.0 to 0.0 and 1.0 to 1.0,
but does not have to remain between these values for all inputs. An easing function can produce
negative values or ones greater than 1.0. For example an easing may overshoot the end value and then
come back again, if it is emulating a spring.

The user provides a callback to interpolate a model between a start and end value using the eased
progress value.

Several animations can be combined inside an `Animation` container. A subscription can be generated
for the `Animation` that will only be active when it contains an animations that have not completed
all of their progress.

\`

-}

import Browser.Events
import Time exposing (Posix)


type State a
    = Ready
        { durationMs : Int
        , easing : Float -> Float
        , interpolate : Float -> a -> a
        }
    | Running
        { progress : Float
        , startMs : Int
        , durationMs : Int
        , easing : Float -> Float
        , interpolate : Float -> a -> a
        }


{-| Animation is a container for 1 or more things that can animate the properies of some model `a`.
-}
type Animation a
    = Animation (List (State a))


{-| given an animation and a function to create a message from a timestamp, will generate a subscription
to listen to the animation frame callback and generate messages when it is ready.

The subscription will only be active if the animation has Running states.

-}
subscriptions : Animation a -> (Posix -> msg) -> Sub msg
subscriptions (Animation states) toMsg =
    case states of
        [] ->
            Sub.none

        _ ->
            Browser.Events.onAnimationFrame toMsg


{-| Creates an empty animation, to act as a container to which more animations can be added.
-}
empty : Animation a
empty =
    Animation []


{-| Adds something to animate to an animation container by specifying:

    * The duration in ms the animation is to run for.
    * An easing function.
    * An interpolation function to update some model.

-}
animate : Int -> (Float -> Float) -> (Float -> a -> a) -> Animation a -> Animation a
animate durationMs easing interpolate (Animation states) =
    Ready
        { durationMs = durationMs
        , easing = easing
        , interpolate = interpolate
        }
        :: states
        |> Animation


{-| Steps the animation, applying its easing functions and interpolations to the current posix timestamp.
-}
step : Posix -> Animation a -> a -> ( Animation a, a )
step posix (Animation states) model =
    let
        nowMs =
            Time.posixToMillis posix
    in
    List.foldr
        (\state ( stateAccum, modelAccum ) ->
            case state of
                Ready { durationMs, easing, interpolate } ->
                    let
                        running =
                            Running
                                { durationMs = durationMs
                                , easing = easing
                                , interpolate = interpolate
                                , progress = 0.0
                                , startMs = nowMs
                                }
                    in
                    ( running :: stateAccum, modelAccum )

                Running { progress, startMs, durationMs, easing, interpolate } ->
                    let
                        nextProgress =
                            toFloat (nowMs - startMs) / toFloat durationMs
                    in
                    if nextProgress >= 1.0 then
                        ( stateAccum, interpolate 1.0 modelAccum )

                    else
                        let
                            running =
                                Running
                                    { durationMs = durationMs
                                    , easing = easing
                                    , interpolate = interpolate
                                    , progress = progress
                                    , startMs = startMs
                                    }
                        in
                        ( running :: stateAccum, interpolate (easing nextProgress) modelAccum )
        )
        ( [], model )
        states
        |> Tuple.mapFirst Animation
