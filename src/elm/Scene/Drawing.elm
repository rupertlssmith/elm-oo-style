module Scene.Drawing exposing (..)

import Animate exposing (Animator, Timeline)
import BoundingBox2d
import Camera2d exposing (Camera2d)
import Config
import Dict exposing (Dict)
import Ease
import Geometry exposing (BLocal, BScreen, PScene, PScreen, Screen, VScene, VScreen)
import GestureEvent exposing (GestureEvent)
import Html as H exposing (Html)
import Html.Attributes as HA
import Params
import Pixels exposing (Pixels)
import Point2d
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
    , gesturesOnDoc : Pointer.Model GestureEvent Msg Screen
    , gesturesOnDiv : Pointer.Model GestureEvent Msg Screen
    , gestureCondition : GestureCondition
    , camera : Timeline (Camera2d Unitless Pixels Geometry.Scene)
    , mousePos : PScreen
    , entities : Dict EntityId (Entity Msg)
    , nextId : EntityId
    , rootId : EntityId
    }


type GestureCondition
    = NoGesture
    | Dragging { prevPos : PScreen }


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

        camera =
            Camera2d.zoomedAt
                Point2d.origin
                (Quantity.rate (Pixels.float Config.config.defaultZoom)
                    (Quantity.float 1.0)
                )
    in
    Spec.scene
        { update = updateScene
        , view = viewScene
        , subscriptions = subscriptionsScene
        , add = addToDrawing
        }
        { window = frame
        , frame = drawingFrameFromWindow frame
        , gesturesOnDoc =
            Pointer.init Nothing
                (OnGestureMsg Doc)
                |> Pointer.apply docPointerHandler
        , gesturesOnDiv =
            Pointer.init Nothing
                (OnGestureMsg Div)
                |> Pointer.apply divPointerHandler
        , gestureCondition = NoGesture
        , camera = Animate.static camera
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
    , Animate.subscriptions animator Tick model
    ]
        |> Sub.batch


animator : Animator Drawing
animator =
    Animate.empty
        |> Animate.animate
            .camera
            (\x m -> { m | camera = x })



-- Transitions on the update context.


toUpdateContext : (Drawing -> Scene) -> Drawing -> UpdateContext Msg
toUpdateContext raise drawing =
    Spec.updateContext
        { frame = \d -> d.frame
        , camera = \d -> d.camera
        , moveCamera = moveCamera
        , zoomToBox = zoomToBox
        , updateEntity = \eid e d -> { d | entities = Dict.insert eid e d.entities }
        , toScene = raise
        }
        drawing


updateContextToScene : UpdateContext Msg -> Scene
updateContextToScene (UpdateContext ctx) =
    ctx.toScene



-- Update


updateScene : Msg -> Drawing -> (Drawing -> Scene) -> ( Scene, Cmd Msg )
updateScene msg drawing raise =
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
            U2.pure drawing
                |> U2.andMap (onDrag args id raise)
                |> Tuple.mapFirst updateContextToScene

        OnGestureDragEnd args { id } _ ->
            U2.pure drawing
                |> U2.andMap (onDragEnd args id raise)
                |> Tuple.mapFirst updateContextToScene

        OnGestureDoubleTap args { id } ->
            U2.pure drawing
                |> U2.andMap (onDoubleTap args id raise)
                |> Tuple.mapFirst updateContextToScene

        OnGestureMove pos _ ->
            U2.pure { drawing | mousePos = pos }
                |> Tuple.mapFirst raise

        Tick newTime ->
            Animate.step newTime animator drawing
                |> U2.pure
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
        camera =
            Animate.value drawing.camera

        zoom =
            Quantity.at_ (Quantity.unsafe 1.0) (Camera2d.zoom camera)
                |> Quantity.toFloat

        newZoom =
            (zoom * wheelEvent.scale)
                |> clamp Config.config.minZoom Config.config.maxZoom

        newCamera =
            Camera2d.setZoomAtScreenPoint
                (Quantity.rate
                    (Pixels.float newZoom)
                    (Quantity.float 1.0)
                )
                wheelEvent.pos
                drawing.frame
                camera
    in
    U2.pure
        { drawing | camera = Animate.staticIfInactive newCamera drawing.camera }


zoomToBox : PScene -> BLocal -> Float -> Drawing -> Drawing
zoomToBox pos bbox scale model =
    let
        camera =
            Animate.value model.camera

        origin =
            Camera2d.origin camera

        translation =
            Vector2d.from origin pos

        largestTargetDim =
            BoundingBox2d.dimensions bbox
                |> Tuple2.uncurry Quantity.max
                |> Quantity.multiplyBy 1.1

        smallestFrameDim =
            BoundingBox2d.dimensions model.frame
                |> Tuple2.uncurry Quantity.min

        zoom =
            Quantity.rate smallestFrameDim largestTargetDim

        target =
            camera
                |> Camera2d.translateBy translation
                |> Camera2d.setZoom zoom
    in
    { model
        | camera =
            Animate.timeline
                { start = camera
                , end = target
                , durationMs = 100
                , easing = Ease.outQuad
                , interpolate = Camera2d.interpolateFrom
                }
    }


moveCamera : VScene -> Drawing -> Drawing
moveCamera vscene model =
    let
        camera =
            Animate.value model.camera |> Camera2d.translateBy vscene
    in
    { model | camera = Animate.staticIfInactive camera model.camera }


onDrag : Pointer.DragArgs Screen -> EntityId -> (Drawing -> Scene) -> Drawing -> ( UpdateContext Msg, Cmd Msg )
onDrag args entityId raise model =
    let
        pp =
            case model.gestureCondition of
                NoGesture ->
                    args.startPos

                Dragging { prevPos } ->
                    prevPos

        camera =
            Animate.value model.camera

        prevPosScene =
            Camera2d.pointToScene camera model.frame pp

        curPosScene =
            Camera2d.pointToScene camera model.frame args.pos

        trans =
            Vector2d.from prevPosScene curPosScene

        context =
            { model | gestureCondition = Dragging { prevPos = args.pos } }
                |> toUpdateContext raise

        newContext =
            Dict.get entityId model.entities
                |> Maybe.map (\(Entity e) -> e.move trans context)
                |> Maybe.withDefault context
    in
    U2.pure newContext


onDragEnd : Pointer.DragArgs Screen -> EntityId -> (Drawing -> Scene) -> Drawing -> ( UpdateContext Msg, Cmd Msg )
onDragEnd _ id raise model =
    { model | gestureCondition = NoGesture }
        |> toUpdateContext raise
        |> U2.pure


onDoubleTap : Pointer.PointArgs Screen -> EntityId -> (Drawing -> Scene) -> Drawing -> ( UpdateContext Msg, Cmd Msg )
onDoubleTap args entityId raise model =
    let
        context =
            toUpdateContext raise model

        newContext =
            Dict.get entityId model.entities
                |> Maybe.map (\(Entity e) -> e.select context)
                |> Maybe.withDefault context
    in
    U2.pure newContext



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
        camera =
            Animate.value drawing.camera

        zoom =
            Quantity.at_ (Quantity.unsafe 1.0) (Camera2d.zoom camera)
                |> Quantity.toFloat

        context : ViewContextIF Msg
        context =
            { noop = Noop
            , zoom = zoom
            , frame = drawing.frame
            , camera = camera
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
         , Camera2d.svgViewBoxWithFocus camera drawing.frame drawing.window
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
