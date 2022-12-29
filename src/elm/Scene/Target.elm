module Scene.Target exposing (..)

import BoundingBox2d
import Camera2d
import Circle2d
import Color
import Geometry exposing (BLocal, PScene, PScreen, Screen)
import Geometry.Svg
import Html.Attributes as HA
import Json.Encode as Encode
import Point2d
import Pointer
import Quantity
import Scene.Spec as Spec exposing (Entity, EntityId, UpdateContext(..), ViewContextIF)
import Tuple2
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types
    exposing
        ( Opacity(..)
        , Paint(..)
        )
import Vector2d


type alias Target =
    { id : EntityId
    , gestureCondition : GestureCondition
    , pos : PScene
    , bbox : BLocal
    }


type GestureCondition
    = NoGesture
    | Moving PScreen


{-| A drawing of a target.
-}
target : EntityId -> Entity msg
target id =
    Spec.entity
        { gestureHandler =
            { tap = Nothing
            , doubleTap = Just onDoubleTap
            , drag = Just onDrag
            , dragEnd = Just onDragEnd
            }
                |> Just
        , move = \_ model -> model
        , animate = \_ _ -> Nothing
        , position = .pos
        , bbox = .bbox
        , view = \context model -> [ view context model ]
        }
        { id = id
        , gestureCondition = NoGesture
        , pos = Point2d.origin
        , bbox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float -125
                , minY = Quantity.float -125
                , maxX = Quantity.float 125
                , maxY = Quantity.float 125
                }
        }


onDoubleTap :
    Pointer.PointArgs Screen
    -> Target
    -> (Target -> Entity msg)
    -> UpdateContext msg
    -> UpdateContext msg
onDoubleTap _ model _ (UpdateContext uc) =
    let
        origin =
            Camera2d.origin uc.camera

        destination =
            model.pos

        translation =
            Vector2d.from origin destination

        targetBBox =
            model.bbox

        largestTargetDim =
            BoundingBox2d.dimensions targetBBox
                |> Tuple2.uncurry Quantity.max
                |> Quantity.multiplyBy 1.1

        smallestFrameDim =
            BoundingBox2d.dimensions uc.frame
                |> Tuple2.uncurry Quantity.min

        zoom =
            Quantity.rate smallestFrameDim largestTargetDim

        targetCamera =
            uc.camera
                |> Camera2d.translateBy translation
                |> Camera2d.setZoom zoom
                |> Camera2d.toZoomSpace
    in
    uc.animateZoomToTarget targetCamera


onDrag :
    Pointer.DragArgs Screen
    -> Target
    -> (Target -> Entity msg)
    -> UpdateContext msg
    -> UpdateContext msg
onDrag args model raise (UpdateContext uc) =
    let
        prevPos =
            case model.gestureCondition of
                NoGesture ->
                    args.startPos

                Moving pos ->
                    pos

        prevPosScene =
            Camera2d.pointToScene uc.camera uc.frame prevPos

        curPosScene =
            Camera2d.pointToScene uc.camera uc.frame args.pos

        trans =
            Vector2d.from prevPosScene curPosScene
    in
    { model
        | gestureCondition = Moving args.pos
        , pos = Point2d.translateBy trans model.pos
    }
        |> raise
        |> uc.updateEntity model.id


onDragEnd :
    Pointer.DragArgs Screen
    -> Target
    -> (Target -> Entity msg)
    -> UpdateContext msg
    -> UpdateContext msg
onDragEnd _ model raise (UpdateContext uc) =
    { model | gestureCondition = NoGesture }
        |> raise
        |> uc.updateEntity model.id


view : ViewContextIF msg -> Target -> Svg msg
view _ model =
    let
        scaleFactor =
            Quantity.per (Quantity.float 1) (Quantity.float 2)
    in
    Svg.g
        [ HA.property "data-drawing-id" (Encode.string model.id)
        , Color.rgb255 40 40 40 |> Paint |> SvgAttr.fill
        , Opacity 1.0 |> SvgAttr.fillOpacity
        , InPx.strokeWidth 0
        ]
        [ Svg.path
            [ SvgAttr.d """M250.803-1C112.311-1-1,111.472-1,250.803s113.311,251.803,251.803,251.803s251.803-113.311,251.803-251.803
                                  S389.295-1,250.803-1z M250.803,485.82c-129.259,0-235.016-105.757-235.016-235.016S121.544,15.787,250.803,15.787
                                  S485.82,121.544,485.82,250.803S380.062,485.82,250.803,485.82z"""
            ]
            []
        , Circle2d.atPoint (Point2d.unitless 251 251) (Quantity.float 236)
            |> Geometry.Svg.circle2d
                [ Color.rgb255 140 140 140 |> Paint |> SvgAttr.fill
                , Opacity 0.5 |> SvgAttr.fillOpacity
                ]
        , Svg.path
            [ SvgAttr.d """M250.803,32.574c-120.026,0-218.229,98.203-218.229,218.229c0,120.866,98.203,218.23,218.229,218.23
                           s218.23-97.364,218.23-218.23C469.033,130.777,370.829,32.574,250.803,32.574z M452.057,242.41h-66.119v-57.915
                           c0-37.771-31.056-67.987-67.987-67.987h-58.754V49.55C363.351,53.875,447.731,138.255,452.057,242.41z M334.738,259.197h34.413
                           v58.754c0,28.538-23.502,51.2-51.2,51.2h-58.754v-34.413c0-5.036-3.357-8.393-8.393-8.393s-8.393,3.357-8.393,8.393v34.413
                           h-57.915c-28.538,0-51.2-23.502-51.2-51.2v-58.754h33.574c5.036,0,8.393-3.357,8.393-8.393s-3.357-8.393-8.393-8.393h-33.574
                           v-57.915c0-28.538,23.502-51.2,51.2-51.2h57.915v33.574c0,5.036,3.357,8.393,8.393,8.393s8.393-3.357,8.393-8.393v-33.574h58.754
                           c28.538,0,51.2,23.502,51.2,51.2v57.915h-34.413c-5.036,0-8.393,3.357-8.393,8.393S329.702,259.197,334.738,259.197z
                           M242.41,49.55v66.958h-57.915c-37.771,0-67.987,31.056-67.987,67.987v57.915H49.55C53.875,138.255,138.255,53.875,242.41,49.55z
                           M49.55,259.197h66.958v58.754c0,36.931,30.216,67.148,67.987,67.148h57.915v66.958
                           C138.255,447.731,53.875,363.351,49.55,259.197z M259.197,452.057v-66.958h57.915c37.77,0,67.987-30.216,68.826-67.148v-58.754
                           h66.119C447.731,363.351,363.351,447.731,259.197,452.057z"""
            ]
            []
        , Svg.path
            [ SvgAttr.d """M284.377,242.41h-25.18v-25.18c0-5.036-3.357-8.393-8.393-8.393s-8.393,3.357-8.393,8.393v25.18h-25.18
                           c-5.036,0-8.393,3.357-8.393,8.393s3.357,8.393,8.393,8.393h25.18v25.18c0,5.036,3.357,8.393,8.393,8.393
                           s8.393-3.357,8.393-8.393v-25.18h25.18c5.036,0,8.393-3.357,8.393-8.393S289.413,242.41,284.377,242.41z"""
            ]
            []
        ]
        |> Geometry.Svg.translateBy (Vector2d.unitless -251 -251)
        |> Geometry.Svg.at_ scaleFactor
        |> Geometry.Svg.translateBy (Vector2d.from Point2d.origin model.pos)
