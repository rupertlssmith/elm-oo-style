module GestureEvent exposing
    ( GestureEvent
    , gestureDecoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Pointer exposing (EventKind)



-- Gesture events describing user pointer actions on the gameboard.


type alias GestureEvent =
    { id : String
    }



-- Codecs


gestureDecoder : String -> EventKind -> Decoder GestureEvent
gestureDecoder rootId _ =
    keyDecoder
        |> Decode.andThen
            (\_ -> Decode.field "target" (clickableDecoder rootId))


keyDecoder : Decoder Bool
keyDecoder =
    Decode.map2
        (\ctrl meta -> ctrl || meta)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)


clickableDecoder : String -> Decoder GestureEvent
clickableDecoder rootId =
    Decode.oneOf
        [ itemDecoder
        , rootDecoder rootId
        , Decode.lazy
            (\_ ->
                clickableDecoder rootId
                    |> Decode.field "parentElement"
            )
        , Decode.succeed { id = rootId }
        ]


itemDecoder : Decoder GestureEvent
itemDecoder =
    Decode.succeed (\id -> { id = id })
        |> DE.andMap (Decode.field "data-drawing-id" Decode.string)


rootDecoder : String -> Decoder GestureEvent
rootDecoder rootId =
    Decode.succeed { id = rootId }
        |> DE.when (Decode.field "id" Decode.string) ((==) rootId)
