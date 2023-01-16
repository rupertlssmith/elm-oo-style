module Params exposing (view)

import Animate exposing (Timeline)
import Camera2d exposing (Camera2d)
import Geometry exposing (BScreen, PScreen, Scene)
import Html as H exposing (Html)
import Html.Attributes as HA
import Json.Encode as Encode
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Unitless)


type alias Params a =
    { a
        | frame : BScreen
        , mousePos : PScreen
        , camera : Timeline (Camera2d Unitless Pixels Geometry.Scene)
    }


view : Params a -> Html msg
view model =
    let
        camera =
            Animate.value model.camera

        zoomPct =
            Quantity.at (Camera2d.zoom camera) (Quantity.float 1.0)
                |> Pixels.toFloat
                |> (*) 100.0
                |> round
                |> String.fromInt

        cameraOrigin =
            Camera2d.origin camera

        pointToString p =
            let
                xy =
                    Point2d.toRecord Quantity.unwrap p
            in
            "{ x = "
                ++ (xy.x |> round |> String.fromInt)
                ++ ", y = "
                ++ (xy.y |> round |> String.fromInt)
                ++ " }"

        origin =
            pointToString cameraOrigin

        mouseScreenPos =
            pointToString model.mousePos

        mouseScenePos =
            model.mousePos
                |> Camera2d.pointToScene camera model.frame
                |> pointToString

        checkPos =
            model.mousePos
                |> Camera2d.pointToScene camera model.frame
                |> Camera2d.pointToScreen camera model.frame
                |> pointToString
    in
    H.div
        [ HA.id "params-container"
        , HA.class "noselect"
        , Encode.string "x" |> HA.property "data-gb-id"
        , Encode.string "none" |> HA.property "data-gb-action"
        ]
        [ H.h3 [] [ H.text "Camera" ]
        , H.div
            []
            [ H.div []
                [ H.text "Zoom "
                , zoomPct ++ "\u{2009}%" |> H.text
                ]
            ]
        , H.div
            []
            [ H.div []
                [ H.text "Origin "
                , origin |> H.text
                ]
            ]
        , H.br [] []
        , H.div
            []
            [ H.div []
                [ H.text "Mouse Screen Position "
                , mouseScreenPos |> H.text
                ]
            ]
        , H.div
            []
            [ H.div []
                [ H.text "Mouse Scene Position "
                , mouseScenePos |> H.text
                ]
            ]
        , H.div
            []
            [ H.div []
                [ H.text "Check Remapped Screen Position "
                , checkPos |> H.text
                ]
            ]
        , H.br [] []
        , H.div [] [ H.text "Double click the target to zoom it." ]
        ]
