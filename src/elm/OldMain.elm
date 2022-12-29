module OldMain exposing (Model, Msg, main)

import Animator exposing (Timeline)
import Array
import BoundingBox2d
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Camera2d exposing (Camera2d, ZoomSpace)
import Circle2d
import Color
import Css.Global
import DGrammer.DGrammar
import Existential
import Geometry exposing (BScene, BScreen, PScreen, Scene, Screen, VScreen)
import Geometry.Svg
import GestureEvent exposing (GestureEvent(..))
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Lazy
import Html.Styled as HS
import Json.Encode as Encode
import Params
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Pointer
import Ports
import Quantity exposing (Unitless)
import Rectangle2d
import Scene.Top
import Style
import Task
import Time exposing (Posix)
import Tuple2
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types
    exposing
        ( Align(..)
        , CoordinateSystem(..)
        , MeetOrSlice(..)
        , Opacity(..)
        , Paint(..)
        , Scale(..)
        , ShapeRendering(..)
        )
import Update2 as U2
import Vector2d


config =
    let
        fontSize =
            30

        lineHeightRatio =
            1.4
    in
    { fontSize = fontSize
    , lineHeight = (lineHeightRatio * fontSize) |> floor |> toFloat
    , zoomStep = 1.1
    , defaultZoom = 1.0
    , maxZoom = 5
    , minZoom = 0.2
    , defaultSize = Vector2d.unitless 400 400
    , noteFontLevels =
        [ 0.5, 0.75, 1, 1.25, 1.5 ]
            |> Array.fromList
    , defaultNoteFontLevel = 2
    , colors =
        { bg = Color.rgb255 225 225 20
        , border = Color.rgb255 225 225 20
        }
    , containerElementId = "zoomable"
    , minimumNoteSize = Vector2d.unitless 60 60
    , editorId = "sticky-1"
    , leftMenuWidth = 50
    , rightOverlayWidth = 250
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = SizingWindow
    | Ready DrawingModel


type alias DrawingModel =
    { window : BScreen
    , frame : BScreen
    , zoom : Float
    , fontLevel : Int
    , gestures : Pointer.Model GestureEvent Msg Screen
    , gesturesOnDiv : Pointer.Model GestureEvent Msg Screen
    , gestureCondition : GestureCondition
    , camera : Camera2d Unitless Pixels Scene
    , testBox : BScene
    , zoomAnimation : Timeline ZoomState
    , mousePos : PScreen
    }


type ZoomState
    = ZoomInactive
    | ZoomStart (Point3d.Point3d Unitless (ZoomSpace Pixels Scene))
    | ZoomTarget (Point3d.Point3d Unitless (ZoomSpace Pixels Scene))


type GestureCondition
    = NoGesture
    | MovingCamera (Camera2d Unitless Pixels Scene)


type Msg
    = WindowSize VScreen
    | OnGestureMsg (Pointer.Msg GestureEvent Screen)
    | OnDivGestureMsg (Pointer.Msg GestureEvent Screen)
    | OnGestureDrag (Pointer.DragArgs Screen) GestureEvent GestureEvent
    | OnGestureDragEnd (Pointer.DragArgs Screen) GestureEvent GestureEvent
    | OnGestureTap (Pointer.PointArgs Screen) GestureEvent
    | OnGestureDoubleTap (Pointer.PointArgs Screen) GestureEvent
    | OnGestureMove PScreen GestureEvent
    | OnGestureZoom (Pointer.ScaleArgs Screen) GestureEvent
    | Tick Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindow
    , Task.perform (viewportToSize >> WindowSize) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Browser.Events.onResize coordsToSize |> Sub.map WindowSize
    , case model of
        Ready drawing ->
            [ Pointer.subscriptions
                { onPointerDown = Ports.onPointerDown
                , onPointerUp = Ports.onPointerUp
                , onPointerMove = Ports.onPointerMove
                , onPointerCancel = Ports.onPointerCancel
                }
                drawing.gestures
                (GestureEvent.gestureDecoder config.containerElementId)
            , case Animator.current drawing.zoomAnimation of
                ZoomInactive ->
                    Sub.none

                _ ->
                    Animator.toSubscription Tick drawing animator
            ]
                |> Sub.batch

        _ ->
            Sub.none
    ]
        |> Sub.batch



-- Window size conversions


coordsToSize : Int -> Int -> VScreen
coordsToSize x y =
    Vector2d.pixels (toFloat x) (toFloat y)


viewportToSize : Viewport -> VScreen
viewportToSize vport =
    Vector2d.pixels vport.viewport.width vport.viewport.height



-- Animation of the Drawing.


animator : Animator.Animator DrawingModel
animator =
    Animator.animator
        |> Animator.watching
            .zoomAnimation
            (\x m -> { m | zoomAnimation = x })



-- Update for the Application lifecycle.


switchModel : (a -> Model) -> a -> ( Model, Cmd Msg )
switchModel cons state =
    ( cons state
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case ( model, Debug.log "msg" msg ) of
    case ( model, msg ) of
        ( SizingWindow, WindowSize windowSize ) ->
            let
                docPointerHandlers =
                    { drag = OnGestureDrag
                    , dragEnd = OnGestureDragEnd
                    , click = OnGestureTap
                    , doubleClick = OnGestureDoubleTap
                    , move = OnGestureMove
                    }

                docPointerHandler =
                    Pointer.empty
                        |> Pointer.onDrag 0 docPointerHandlers
                        |> Pointer.onClick 0 docPointerHandlers
                        |> Pointer.onDoubleClick 0 docPointerHandlers
                        |> Pointer.onMove docPointerHandlers

                divPointerHandlers =
                    { wheel = OnGestureZoom
                    , pinch = OnGestureZoom
                    }

                divPointerHandler =
                    Pointer.empty
                        |> Pointer.onWheel divPointerHandlers
                        |> Pointer.onPinch divPointerHandlers

                frame =
                    windowSizeToBBox windowSize
            in
            U2.pure
                (Ready
                    { window = frame
                    , frame = drawingFrameFromWindow frame
                    , zoom = config.defaultZoom
                    , fontLevel = config.defaultNoteFontLevel
                    , gestures =
                        Pointer.init Nothing
                            OnGestureMsg
                            |> Pointer.apply docPointerHandler
                    , gesturesOnDiv =
                        Pointer.init Nothing
                            OnDivGestureMsg
                            |> Pointer.apply divPointerHandler
                    , gestureCondition = NoGesture
                    , camera =
                        Camera2d.zoomedAt
                            Point2d.origin
                            (Quantity.rate (Pixels.float config.defaultZoom)
                                (Quantity.float 1.0)
                            )
                    , testBox =
                        BoundingBox2d.fromExtrema
                            { minX = -40.0 |> Quantity.float
                            , minY = -25.0 |> Quantity.float
                            , maxX = 40.0 |> Quantity.float
                            , maxY = 25.0 |> Quantity.float
                            }
                    , zoomAnimation = Animator.init ZoomInactive
                    , mousePos = Point2d.origin
                    }
                )

        ( Ready drawing, _ ) ->
            U2.pure drawing
                |> U2.andThen (updateReady msg)
                |> U2.andMap (switchModel Ready)

        _ ->
            U2.pure model


windowSizeToBBox : VScreen -> BScreen
windowSizeToBBox size =
    BoundingBox2d.from
        (Point2d.pixels config.leftMenuWidth 0)
        (Point2d.xy (Vector2d.xComponent size) (Vector2d.yComponent size))


drawingFrameFromWindow : BScreen -> BScreen
drawingFrameFromWindow window =
    let
        extrema =
            BoundingBox2d.extrema window

        -- Make space for the right hand side overlay.
        adjusted =
            { extrema
                | maxX =
                    Quantity.minus
                        (Pixels.float config.rightOverlayWidth)
                        extrema.maxX
            }
    in
    BoundingBox2d.fromExtrema adjusted



-- Update for the Ready state.


updateReady : Msg -> DrawingModel -> ( DrawingModel, Cmd Msg )
updateReady msg drawing =
    case ( drawing.gestureCondition, msg ) of
        ( _, WindowSize windowSize ) ->
            U2.pure { drawing | window = windowSizeToBBox windowSize }

        ( _, OnGestureMsg gestureMsg ) ->
            U2.pure drawing
                |> U2.andThen (processGesture gestureMsg)

        ( _, OnDivGestureMsg gestureMsg ) ->
            U2.pure drawing
                |> U2.andThen (processDivGesture gestureMsg)

        ( _, OnGestureZoom args _ ) ->
            U2.pure drawing
                |> U2.andThen (adjustZoom args)

        ( NoGesture, OnGestureDrag args _ _ ) ->
            U2.pure drawing
                |> U2.andThen (moveCamera args drawing.camera)

        ( MovingCamera cameraStart, OnGestureDrag args _ _ ) ->
            U2.pure drawing
                |> U2.andThen (moveCamera args cameraStart)

        ( _, OnGestureDragEnd _ _ _ ) ->
            U2.pure drawing
                |> U2.andThen resetGestureCondition

        ( _, OnGestureDoubleTap _ (ItemWithId { id }) ) ->
            if id == "testBox" then
                U2.pure drawing
                    |> U2.andThen animateZoomToTarget

            else
                U2.pure drawing

        ( _, OnGestureMove pos _ ) ->
            U2.pure { drawing | mousePos = pos }

        ( _, Tick newTime ) ->
            Animator.update newTime animator drawing
                |> U2.pure
                |> U2.andThen animateCamera

        _ ->
            U2.pure drawing


processGesture : Pointer.Msg GestureEvent Screen -> DrawingModel -> ( DrawingModel, Cmd Msg )
processGesture gestureMsg drawing =
    let
        ( newGesturesModel, gestureCmds ) =
            Pointer.update gestureMsg drawing.gestures
    in
    ( { drawing | gestures = newGesturesModel }
    , gestureCmds
    )


processDivGesture : Pointer.Msg GestureEvent Screen -> DrawingModel -> ( DrawingModel, Cmd Msg )
processDivGesture gestureMsg drawing =
    let
        ( newGesturesModel, gestureCmds ) =
            Pointer.update gestureMsg drawing.gesturesOnDiv
    in
    ( { drawing | gesturesOnDiv = newGesturesModel }
    , gestureCmds
    )


adjustZoom : Pointer.ScaleArgs Screen -> DrawingModel -> ( DrawingModel, Cmd Msg )
adjustZoom wheelEvent drawing =
    let
        newZoom =
            (drawing.zoom * wheelEvent.scale)
                |> clamp config.minZoom config.maxZoom
    in
    U2.pure
        { drawing
            | zoom = newZoom
            , camera =
                Camera2d.setZoomAtScreenPoint
                    (Quantity.rate
                        (Pixels.float newZoom)
                        (Quantity.float 1.0)
                    )
                    wheelEvent.pos
                    drawing.frame
                    drawing.camera
        }


resetGestureCondition : DrawingModel -> ( DrawingModel, Cmd Msg )
resetGestureCondition drawing =
    { drawing | gestureCondition = NoGesture }
        |> U2.pure


animateZoomToTarget : DrawingModel -> ( DrawingModel, Cmd Msg )
animateZoomToTarget drawing =
    let
        origin =
            Camera2d.origin drawing.camera

        destination =
            Point2d.origin

        translation =
            Vector2d.from origin destination

        targetBBox =
            BoundingBox2d.fromExtrema
                { minX = Quantity.float -125
                , minY = Quantity.float -125
                , maxX = Quantity.float 125
                , maxY = Quantity.float 125
                }

        largestTargetDim =
            BoundingBox2d.dimensions targetBBox
                |> Tuple2.uncurry Quantity.max
                |> Quantity.multiplyBy 1.1

        smallestFrameDim =
            BoundingBox2d.dimensions drawing.frame
                |> Tuple2.uncurry Quantity.min

        zoom =
            Quantity.rate smallestFrameDim largestTargetDim

        targetCamera =
            drawing.camera
                |> Camera2d.translateBy translation
                |> Camera2d.setZoom zoom

        -- Derive the camera start and end positions in ZoomSpace.
        startZoomSpace =
            Camera2d.toZoomSpace drawing.camera
                |> ZoomStart

        targetZoomSpace =
            Camera2d.toZoomSpace targetCamera
                |> ZoomTarget
    in
    { drawing
        | zoomAnimation =
            Animator.init startZoomSpace
                |> Animator.go Animator.quickly targetZoomSpace
    }
        |> U2.pure


animateCamera : DrawingModel -> ( DrawingModel, Cmd Msg )
animateCamera drawing =
    let
        zoomSpace =
            Animator.xyz drawing.zoomAnimation
                (\state ->
                    case state of
                        ZoomInactive ->
                            Point3d.origin |> Point3d.toUnitless |> xyzToMovement

                        ZoomStart zs ->
                            zs |> Point3d.toUnitless |> xyzToMovement

                        ZoomTarget zs ->
                            zs |> Point3d.toUnitless |> xyzToMovement
                )
                |> Point3d.fromUnitless

        camera =
            Camera2d.fromZoomSpace zoomSpace

        zoom =
            Quantity.at_ (Quantity.unsafe 1.0) (Camera2d.zoom camera)
                |> Quantity.toFloat
    in
    case Animator.arrived drawing.zoomAnimation of
        ZoomStart _ ->
            U2.pure
                { drawing
                    | camera = camera
                    , zoom = zoom
                }

        _ ->
            U2.pure { drawing | zoomAnimation = Animator.init ZoomInactive }


xyzToMovement : { x : Float, y : Float, z : Float } -> { x : Animator.Movement, y : Animator.Movement, z : Animator.Movement }
xyzToMovement xyz =
    { x = Animator.at xyz.x
    , y = Animator.at xyz.y
    , z = Animator.at xyz.z
    }



-- Camera Control


type alias CameraControl a =
    { a
        | frame : BScreen
        , camera : Camera2d Unitless Pixels Scene
        , gestureCondition : GestureCondition
    }


moveCamera :
    Pointer.DragArgs Screen
    -> Camera2d Unitless Pixels Scene
    -> CameraControl a
    -> ( CameraControl a, Cmd Msg )
moveCamera args cameraStart model =
    { model
        | gestureCondition = MovingCamera cameraStart
        , camera =
            Camera2d.translateByScreenVector
                (Vector2d.from args.pos args.startPos)
                cameraStart
    }
        |> U2.pure



-- Styling


offWhite =
    Color.rgb255 248 240 245



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "SVG Drawing Example"
    , body =
        [ Style.global config |> Css.Global.global |> HS.toUnstyled
        , body model
        ]
    }


body : Model -> Html Msg
body model =
    H.div
        []
        [ Html.Lazy.lazy fullBody model
        ]


fullBody : Model -> Html Msg
fullBody model =
    case model of
        Ready drawing ->
            H.div
                [ HA.id "top-container"
                ]
                [ Params.view drawing
                , leftMenu
                , svgDrawing drawing
                , rightOverlay
                ]

        _ ->
            H.div [] []


leftMenu : Html msg
leftMenu =
    H.div [ HA.id "left-menu" ]
        []


rightOverlay : Html msg
rightOverlay =
    H.div [ HA.id "right-overlay" ]
        []


svgDrawing : DrawingModel -> Svg Msg
svgDrawing drawing =
    Svg.svg
        ([ SvgAttr.preserveAspectRatio (Align ScaleMid ScaleMid) Meet
         , Camera2d.svgViewBoxWithFocus drawing.camera drawing.frame drawing.window
         , SvgCore.svgNamespace
         , SvgAttr.shapeRendering RenderGeometricPrecision
         , HA.id "svg-drawing"
         ]
            ++ Pointer.on drawing.gesturesOnDiv (GestureEvent.gestureDecoder config.containerElementId)
        )
        [ gridPattern drawing
        , background drawing
        , testBox drawing
        ]


gridPattern : DrawingModel -> Svg msg
gridPattern drawing =
    let
        start =
            logBase 10 (1 / drawing.zoom)
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
                    Paint offWhite |> SvgAttr.fill

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


background : DrawingModel -> Svg msg
background { frame } =
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


testBox : DrawingModel -> Svg msg
testBox _ =
    let
        scaleFactor =
            Quantity.per (Quantity.float 1) (Quantity.float 2)
    in
    Svg.g
        [ HA.property "data-drawing-id" (Encode.string "testBox")
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
