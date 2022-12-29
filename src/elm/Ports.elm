port module Ports exposing (..)

import Json.Encode exposing (Value)



-- HTML Pointer API


port onPointerDown : (Value -> msg) -> Sub msg


port onPointerUp : (Value -> msg) -> Sub msg


port onPointerMove : (Value -> msg) -> Sub msg


port onPointerCancel : (Value -> msg) -> Sub msg
