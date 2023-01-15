module Animation exposing
    ( Timeline, timeline, value
    , Animator, empty, animate
    , subscriptions, step
    )

{-| A simple and minimal animation sequencer.

A `Timeline` interpolates properties of a model based on a progress value that runs from 0.0 to 1.0,
in a given length of time.

An easing function can be applied to this. An easing function should map 0.0 to 0.0 and 1.0 to 1.0,
but does not have to remain between these values for all other inputs. An easing function can produce
negative values or ones greater than 1.0. For example an easing may overshoot the end value and then
come back again, if it is emulating a spring.

The user provides a callback to interpolate a model between a start and end value using the eased
progress value.

Several timelines can be combined inside an `Animator` container. A subscription can be generated
for the animator that will only be active when it contains a timeline that has not completed
all of its progress. When the subscription is active is will produce messages with timestamps
provided by `Browser.Events.onAnimationFrame`.

The active state messages can be applied to update the model using the animator and the `step`
function.


# Build a timeline and get the animated value.

@docs Timeline, timeline, value


# Build an animator of timelines.

@docs Animator, empty, animate


# Subscribe to the animation frame, and step the model.

@docs subscriptions, step

-}

import Browser.Events
import Time exposing (Posix)


{-| A Timeline describes the timeline of something being animated.

The timeline has a start time and value and an end time and value. Between those times, the value
will be interpolated between its start and end values by a user supplied interpolation function.

Outside of the period between the start and end times, the timeline will be considerd inactive,
and no timer subscription created for it.

-}
type Timeline a
    = Ready
        { durationMs : Int
        , easing : Float -> Float
        , start : a
        , end : a
        , interpolate : Float -> a -> a -> a
        }
    | Running
        { startMs : Int
        , durationMs : Int
        , easing : Float -> Float
        , start : a
        , end : a
        , interpolate : Float -> a -> a -> a
        , curValue : a
        }
    | Complete { curValue : a }


{-| The Animator is a set of functions that know how to update the timelines in some model.
-}
type Animator mdl
    = Animator (mdl -> Bool) (Int -> mdl -> mdl)


{-| given an animation and a function to create a message from a timestamp, will generate a subscription
to listen to the animation frame callback and generate messages when it is ready.

The subscription will only be active if the animation has Running states.

-}
subscriptions : mdl -> Animator mdl -> (Posix -> msg) -> Sub msg
subscriptions model (Animator isActive _) toMsg =
    if isActive model then
        Browser.Events.onAnimationFrame toMsg

    else
        Sub.none


{-| Creates an empty animator, to act as a container to which more timelines can be added.
-}
empty : Animator mdl
empty =
    Animator (always False) (always identity)


{-| Adds a timeline to animate to the Animator. Functions to extract and update the timeline
on some model must be given.
-}
animate : (mdl -> Timeline a) -> (Timeline a -> mdl -> mdl) -> Animator mdl -> Animator mdl
animate getter setter (Animator isActive stepModel) =
    let
        nextIsActive model =
            case getter model of
                Ready _ ->
                    True

                Running _ ->
                    True

                Complete _ ->
                    isActive model

        nextStepModel nowMs model =
            case getter model of
                Ready { durationMs, easing, start, end, interpolate } ->
                    let
                        running =
                            Running
                                { durationMs = durationMs
                                , easing = easing
                                , startMs = nowMs
                                , start = start
                                , end = end
                                , interpolate = interpolate
                                , curValue = start
                                }
                    in
                    setter running model
                        |> stepModel nowMs

                Running { startMs, durationMs, easing, start, end, interpolate } ->
                    let
                        nextProgress =
                            toFloat (nowMs - startMs) / toFloat durationMs
                    in
                    if nextProgress >= 1.0 then
                        setter
                            (Running
                                { durationMs = durationMs
                                , easing = easing
                                , startMs = nowMs
                                , start = start
                                , end = end
                                , interpolate = interpolate
                                , curValue = interpolate nextProgress start end
                                }
                            )
                            model
                            |> stepModel nowMs

                    else
                        let
                            endValue =
                                interpolate 1.0 start end
                        in
                        setter (Complete { curValue = endValue }) model
                            |> stepModel nowMs

                Complete _ ->
                    stepModel nowMs model
    in
    Animator nextIsActive nextStepModel


{-| Creates an animation Timeline by specifying:

    * The duration in milliseconds the animation is to run for.
    * An easing function (use `identity` is no easing is required).
    * An interpolation function to update some model.
    * The start and end states to animate betwen.

-}
timeline :
    { durationMs : Int
    , easing : Float -> Float
    , start : a
    , end : a
    , interpolate : Float -> a -> a -> a
    }
    -> Timeline a
timeline animSpec =
    Ready animSpec


{-| Gets the current value from a timeline.
-}
value : Timeline a -> a
value tl =
    case tl of
        Ready { start } ->
            start

        Running { curValue } ->
            curValue

        Complete { curValue } ->
            curValue


{-| Steps the animation, applying its easing functions and interpolations to the current posix timestamp.
-}
step : Posix -> Animator mdl -> mdl -> mdl
step posix (Animator _ stepModel) model =
    stepModel (Time.posixToMillis posix) model
