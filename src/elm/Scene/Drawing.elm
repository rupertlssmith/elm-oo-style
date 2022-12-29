module Scene.Drawing exposing (..)

import Animator exposing (Timeline)
import BoundingBox2d
import Camera2d exposing (Camera2d, ZoomSpace)
import Config
import Dict exposing (Dict)
import Geometry exposing (BScreen, PScreen, Screen, VScreen)
import GestureEvent exposing (GestureEvent)
import Html as H exposing (Html)
import Html.Attributes as HA
import Params
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Pointer
import Ports
import Quantity exposing (Unitless)
import Scene.Spec as Spec
    exposing
        ( Entity(..)
        , EntityId
        , GestureLocation(..)
        , Msg(..)
        , Scene(..)
        , UpdateContext(..)
        , ViewContextIF
        )
import Tuple2
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types
    exposing
        ( Align(..)
        , MeetOrSlice(..)
        , Scale(..)
        , ShapeRendering(..)
        )
import Update2 as U2
import Vector2d



-- The entire Scene as a TEA component.


init : VScreen -> EntityId -> ( Scene, Cmd Msg )
init windowSize rootId =
    ( empty windowSize rootId, Cmd.none )


subscriptions : Scene -> Sub Msg
subscriptions (Scene scene) =
    scene.subscriptions


update : Msg -> Scene -> ( Scene, Cmd Msg )
update msg (Scene scene) =
    scene.update msg


view : Scene -> Html Msg
view (Scene scene) =
    scene.view ()



-- The Scene implementation.


type alias Drawing =
    { window : BScreen
    , frame : BScreen
    , zoom : Float
    , gesturesOnDoc : Pointer.Model GestureEvent Msg Screen
    , gesturesOnDiv : Pointer.Model GestureEvent Msg Screen
    , camera : Camera2d Unitless Pixels Geometry.Scene
    , zoomAnimation : Timeline ZoomState
    , mousePos : PScreen
    , entities : Dict EntityId (Entity Msg)
    , nextId : EntityId
    , rootId : EntityId
    }


type ZoomState
    = ZoomInactive
    | ZoomStart (Point3d.Point3d Unitless (ZoomSpace Pixels Geometry.Scene))
    | ZoomTarget (Point3d.Point3d Unitless (ZoomSpace Pixels Geometry.Scene))


empty : VScreen -> EntityId -> Scene
empty windowSize rootId =
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

        addToDrawing id entity drawing =
            { drawing
                | entities = Dict.insert id entity drawing.entities
            }
    in
    Spec.scene
        { update = updateScene
        , view = viewScene
        , subscriptions = subscriptionsScene
        , add = addToDrawing
        }
        { window = frame
        , frame = drawingFrameFromWindow frame
        , zoom = Config.config.defaultZoom
        , gesturesOnDoc =
            Pointer.init Nothing
                (OnGestureMsg Doc)
                |> Pointer.apply docPointerHandler
        , gesturesOnDiv =
            Pointer.init Nothing
                (OnGestureMsg Div)
                |> Pointer.apply divPointerHandler
        , camera =
            Camera2d.zoomedAt
                Point2d.origin
                (Quantity.rate (Pixels.float Config.config.defaultZoom)
                    (Quantity.float 1.0)
                )
        , zoomAnimation = Animator.init ZoomInactive
        , mousePos = Point2d.origin
        , entities = Dict.empty
        , nextId = ""
        , rootId = rootId
        }


add : EntityId -> Entity Msg -> Scene -> Scene
add id entity (Scene scene) =
    scene.add id entity



-- Subscriptions


subscriptionsScene : Drawing -> Sub Msg
subscriptionsScene model =
    [ Pointer.subscriptions
        { onPointerDown = Ports.onPointerDown
        , onPointerUp = Ports.onPointerUp
        , onPointerMove = Ports.onPointerMove
        , onPointerCancel = Ports.onPointerCancel
        }
        model.gesturesOnDoc
        (GestureEvent.gestureDecoder Config.config.containerElementId)
    , case Animator.current model.zoomAnimation of
        ZoomInactive ->
            Sub.none

        _ ->
            Animator.toSubscription Tick model animator
    ]
        |> Sub.batch


animator : Animator.Animator Drawing
animator =
    Animator.animator
        |> Animator.watching
            .zoomAnimation
            (\x m -> { m | zoomAnimation = x })



-- Update


updateScene : Msg -> Drawing -> (Drawing -> Scene) -> ( Scene, Cmd Msg )
updateScene msg drawing raise =
    let
        context : UpdateContext Msg
        context =
            Spec.updateContext
                { frame = \d -> d.frame
                , camera = \d -> d.camera
                , setCamera = \camera d -> { d | camera = camera }
                , animateZoomToTarget = animateZoomToTarget
                , updateEntity = \eid e d -> { d | entities = Dict.insert eid e d.entities }
                , toScene = raise
                }
                drawing

        contextToScene (UpdateContext ctx) =
            ctx.toScene

        handleGesture gestureArgs entityId accessor =
            Dict.get entityId drawing.entities
                |> Maybe.map
                    (\(Entity e) ->
                        e.gestureHandler
                            |> Maybe.map
                                (\handlers ->
                                    accessor handlers
                                        |> Maybe.map (\handler -> handler gestureArgs context)
                                        |> Maybe.withDefault context
                                )
                            |> Maybe.withDefault context
                    )
                |> Maybe.withDefault context
    in
    case msg of
        WindowSize windowSize ->
            U2.pure { drawing | window = windowSizeToBBox windowSize }
                |> Tuple.mapFirst raise

        OnGestureMsg loc gestureMsg ->
            U2.pure drawing
                |> U2.andThen (processGesture loc gestureMsg)
                |> Tuple.mapFirst raise

        OnGestureZoom args _ ->
            U2.pure drawing
                |> U2.andThen (adjustZoom args)
                |> Tuple.mapFirst raise

        OnGestureDrag args { id } _ ->
            U2.pure (handleGesture args id .drag)
                |> Tuple.mapFirst contextToScene

        OnGestureDragEnd args { id } _ ->
            U2.pure (handleGesture args id .dragEnd)
                |> Tuple.mapFirst contextToScene

        OnGestureTap args { id } ->
            U2.pure (handleGesture args id .tap)
                |> Tuple.mapFirst contextToScene

        OnGestureDoubleTap args { id } ->
            U2.pure (handleGesture args id .doubleTap)
                |> Tuple.mapFirst contextToScene

        OnGestureMove pos _ ->
            U2.pure { drawing | mousePos = pos }
                |> Tuple.mapFirst raise

        Tick newTime ->
            Animator.update newTime animator drawing
                |> U2.pure
                |> U2.andThen animateCamera
                |> Tuple.mapFirst raise

        _ ->
            U2.pure drawing
                |> Tuple.mapFirst raise


processGesture :
    GestureLocation
    -> Pointer.Msg GestureEvent Screen
    -> Drawing
    -> ( Drawing, Cmd Msg )
processGesture loc gestureMsg model =
    let
        ( get, set ) =
            case loc of
                Doc ->
                    ( .gesturesOnDoc, \x m -> { m | gesturesOnDoc = x } )

                Div ->
                    ( .gesturesOnDiv, \x m -> { m | gesturesOnDiv = x } )

        ( newGesturesModel, gestureCmds ) =
            Pointer.update gestureMsg (get model)
    in
    ( set newGesturesModel model, gestureCmds )


adjustZoom : Pointer.ScaleArgs Screen -> Drawing -> ( Drawing, Cmd Msg )
adjustZoom wheelEvent drawing =
    let
        newZoom =
            (drawing.zoom * wheelEvent.scale)
                |> clamp Config.config.minZoom Config.config.maxZoom
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


animateCamera : Drawing -> ( Drawing, Cmd Msg )
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


xyzToMovement :
    { x : Float, y : Float, z : Float }
    -> { x : Animator.Movement, y : Animator.Movement, z : Animator.Movement }
xyzToMovement xyz =
    { x = Animator.at xyz.x
    , y = Animator.at xyz.y
    , z = Animator.at xyz.z
    }


animateZoomToTarget : Point3d Unitless (ZoomSpace Pixels Geometry.Scene) -> Drawing -> Drawing
animateZoomToTarget targetZoomSpace drawing =
    let
        -- Derive the animation start and end states through ZoomSpace.
        start =
            Camera2d.toZoomSpace drawing.camera
                |> ZoomStart

        target =
            targetZoomSpace
                |> ZoomTarget
    in
    { drawing
        | zoomAnimation =
            Animator.init start
                |> Animator.go Animator.quickly target
    }



-- View


viewScene : Drawing -> Html Msg
viewScene model =
    H.div [ HA.id "drawing-container" ]
        [ Params.view model
        , svgDrawing model
        ]


svgDrawing : Drawing -> Svg Msg
svgDrawing drawing =
    let
        context : ViewContextIF Msg
        context =
            { noop = Noop
            , zoom = drawing.zoom
            , frame = drawing.frame
            , camera = drawing.camera
            , mousePos = drawing.mousePos
            }

        scene =
            Dict.foldl
                (\_ (Entity entity) accum -> accum ++ entity.view context)
                []
                drawing.entities
    in
    Svg.svg
        ([ SvgAttr.preserveAspectRatio (Align ScaleMid ScaleMid) Meet
         , Camera2d.svgViewBoxWithFocus drawing.camera drawing.frame drawing.window
         , SvgCore.svgNamespace
         , SvgAttr.shapeRendering RenderGeometricPrecision
         , HA.id drawing.rootId
         ]
            ++ Pointer.on drawing.gesturesOnDiv (GestureEvent.gestureDecoder Config.config.containerElementId)
        )
        scene


windowSizeToBBox : VScreen -> BScreen
windowSizeToBBox size =
    BoundingBox2d.from
        (Point2d.pixels Config.config.leftMenuWidth 0)
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
                        (Pixels.float Config.config.rightOverlayWidth)
                        extrema.maxX
            }
    in
    BoundingBox2d.fromExtrema adjusted