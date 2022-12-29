module Scene.Root exposing (..)

{-| The root element of the SVG drawing.

This also acts as the camera controller for pan and zoom.

-}

import BoundingBox2d
import Camera2d exposing (Camera2d, ZoomSpace)
import Color
import Geometry exposing (PScreen, Scene, Screen)
import Geometry.Svg
import GestureEvent exposing (GestureEvent)
import Html.Attributes as HA
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Pointer
import Quantity exposing (Unitless)
import Rectangle2d
import Scene.Spec as Spec exposing (Entity, EntityId, UpdateContext(..), UpdateContextIF, ViewContextIF)
import Style
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types
    exposing
        ( CoordinateSystem(..)
        , Opacity(..)
        , Paint(..)
        )
import Vector2d


type alias Root =
    { id : EntityId
    , gestureCondition : GestureCondition
    }


type GestureCondition
    = NoGesture
    | MovingCamera PScreen


root : EntityId -> Entity msg
root id =
    Spec.entity
        { gestureHandler =
            { tap = Nothing
            , doubleTap = Nothing
            , drag = Just onDrag
            , dragEnd = Just onDragEnd
            }
                |> Just
        , move = \_ drawing -> drawing
        , animate = \_ _ -> Nothing
        , position = always Point2d.origin
        , bbox = always (BoundingBox2d.from Point2d.origin Point2d.origin)
        , view =
            \context model ->
                [ gridPattern context model
                , background context model
                ]
        }
        { id = id
        , gestureCondition = NoGesture
        }


onDrag :
    Pointer.DragArgs Screen
    -> Root
    -> (Root -> Entity msg)
    -> UpdateContext msg
    -> UpdateContext msg
onDrag args model raise (UpdateContext uc) =
    let
        prevPos =
            case model.gestureCondition of
                NoGesture ->
                    args.startPos

                MovingCamera pos ->
                    pos

        camera =
            Camera2d.translateByScreenVector
                (Vector2d.from args.pos prevPos)
                uc.camera

        e =
            { model | gestureCondition = MovingCamera args.pos }
                |> raise
    in
    uc.setCamera camera
        |> Spec.ucAndThen (Spec.updateEntity model.id e)


onDragEnd :
    Pointer.DragArgs Screen
    -> Root
    -> (Root -> Entity msg)
    -> UpdateContext msg
    -> UpdateContext msg
onDragEnd _ model raise (UpdateContext uc) =
    { model | gestureCondition = NoGesture }
        |> raise
        |> uc.updateEntity model.id


gridPattern : ViewContextIF msg -> Root -> Svg msg
gridPattern context _ =
    let
        start =
            logBase 10 (1 / context.zoom)
                + 2
                |> ceiling

        end =
            start + 2
    in
    gridPatterns start end |> Svg.defs []


gridPatterns : Int -> Int -> List (Svg msg)
gridPatterns pow end =
    gridPatternsInner pow pow end []


gridPatternsInner : Int -> Int -> Int -> List (Svg msg) -> List (Svg msg)
gridPatternsInner start pow end accum =
    let
        exp =
            5

        tenpow =
            exp ^ pow |> toFloat

        isTopGrid =
            pow >= (end - 1)

        isBottomGrid =
            pow == start

        gridName =
            if isTopGrid then
                "grid"

            else
                "grid" ++ (tenpow |> round |> String.fromInt)
    in
    if pow >= end then
        accum

    else
        Svg.pattern
            [ HA.id gridName
            , InPx.width tenpow
            , InPx.height tenpow
            , InPx.x 0
            , InPx.y 0
            , SvgAttr.patternUnits CoordinateSystemUserSpaceOnUse
            ]
            [ Svg.rect
                [ InPx.width tenpow
                , InPx.height tenpow
                , InPx.x 0
                , InPx.y 0
                , Color.rgb255 176 176 176 |> Paint |> SvgAttr.stroke
                , InPx.strokeWidth (tenpow / 1000)
                , if isBottomGrid then
                    Paint Style.offWhite |> SvgAttr.fill

                  else
                    let
                        innerGridName =
                            "grid" ++ (exp ^ (pow - 1) |> String.fromInt)
                    in
                    Reference innerGridName |> SvgAttr.fill
                ]
                []
            ]
            :: accum
            |> gridPatternsInner start (pow + 1) end


background : ViewContextIF msg -> Root -> Svg msg
background { frame } _ =
    let
        skirtScale =
            20

        ( w, h ) =
            BoundingBox2d.dimensions frame
                |> Tuple.mapBoth Pixels.toFloat Pixels.toFloat

        alignToPattern x =
            (x / 625 |> ceiling |> toFloat) * 625

        x1 =
            -(skirtScale * w |> alignToPattern)

        y1 =
            -(skirtScale * h |> alignToPattern)

        x2 =
            skirtScale * w |> alignToPattern

        y2 =
            skirtScale * h |> alignToPattern

        bgArea =
            Rectangle2d.with
                { x1 = x1 |> Quantity.float
                , y1 = y1 |> Quantity.float
                , x2 = x2 |> Quantity.float
                , y2 = y2 |> Quantity.float
                }
    in
    Geometry.Svg.rectangle2d
        [ SvgAttr.fill <| Reference "grid"
        , SvgAttr.fillOpacity <| Opacity 1.0
        , InPx.strokeWidth 0
        ]
        bgArea
