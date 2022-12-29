module Geometry exposing
    ( Screen, Scene, Local
    , BLocal, BScreen, BScene
    , FLocal, FScene
    , PScreen, PScene
    , RScreen, RScene
    , VScreen, VScene
    , encodeRScene, encodePScene, encodeVScene
    , pScreenDecoder, rSceneDecoder, vSceneDecoder, pSceneDecoder, bLocalDecoder
    , rectToXywh
    , bboxToRect
    , minimumRectSize, rectDeltaSize, rectSetSize, fitRectToMaxSize, bboxToScene, bboxToLocal
    , pointToSceneOrigin
    , rectToXywhPixels
    )

{-| Contains Commonly used Geometry definitions and functions.


# Common Geometry types.

@docs Screen, Scene, Local
@docs BLocal, BScreen, BScene
@docs FLocal, FScene
@docs PScreen, PScene
@docs RScreen, RScene
@docs VScreen, VScene


# JSON Codecs

@docs encodeRScene, encodePScene, encodeVScene
@docs pScreenDecoder, rSceneDecoder, vSceneDecoder, pSceneDecoder, bLocalDecoder


# Conversion

@docs rectToXywh
@docs bboxToRect


# Geometry helper functions

@docs minimumRectSize, rectDeltaSize, rectSetSize, fitRectToMaxSize, bboxToScene, bboxToLocal
@docs pointToSceneOrigin

-}

import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Extra as DE
import Json.Encode as Encode
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Rectangle2d exposing (Rectangle2d)
import Vector2d exposing (Vector2d)



-- Geometry


{-| Local coordinages, for example, on a element within the scene.
-}
type Local
    = Local


{-| Scene coordinates.
-}
type Scene
    = Scene


{-| Screen coordinates.
-}
type Screen
    = Screen


type alias BLocal =
    BoundingBox2d Unitless Local


type alias FLocal =
    Frame2d Unitless Scene { defines : Local }


type alias FScene =
    Frame2d Unitless Scene { defines : Screen }


type alias BScene =
    BoundingBox2d Unitless Scene


type alias PScene =
    Point2d Unitless Scene


type alias RScene =
    Rectangle2d Unitless Scene


type alias VScene =
    Vector2d Unitless Scene


type alias BScreen =
    BoundingBox2d Pixels Screen


type alias PScreen =
    Point2d Pixels Screen


type alias RScreen =
    Rectangle2d Pixels Screen


type alias VScreen =
    Vector2d Pixels Screen


xyDecoder : (Float -> Float -> xy) -> Decoder xy
xyDecoder val =
    Decode.succeed val
        |> DE.andMap (Decode.field "x" Decode.float)
        |> DE.andMap (Decode.field "y" Decode.float)


extremaDecoder : (Float -> Float -> Float -> Float -> p2p) -> Decoder p2p
extremaDecoder val =
    Decode.succeed val
        |> DE.andMap (Decode.field "minX" Decode.float)
        |> DE.andMap (Decode.field "minY" Decode.float)
        |> DE.andMap (Decode.field "maxX" Decode.float)
        |> DE.andMap (Decode.field "maxY" Decode.float)


vSceneDecoder : Decoder VScene
vSceneDecoder =
    xyDecoder Vector2d.unitless


pSceneDecoder : Decoder PScene
pSceneDecoder =
    xyDecoder Point2d.unitless


pScreenDecoder : Decoder PScreen
pScreenDecoder =
    xyDecoder Point2d.pixels


bLocalDecoder : Decoder BLocal
bLocalDecoder =
    extremaDecoder
        (\minX minY maxX maxY ->
            { minX = Quantity.float minX
            , minY = Quantity.float minY
            , maxX = Quantity.float maxX
            , maxY = Quantity.float maxY
            }
                |> BoundingBox2d.fromExtrema
        )


encodeXy : { x : Float, y : Float } -> Value
encodeXy pos =
    Encode.object
        [ ( "x", Encode.float pos.x )
        , ( "y", Encode.float pos.y )
        ]


encodeVScene : VScene -> Value
encodeVScene val =
    Vector2d.toUnitless val |> encodeXy


encodePScene : PScene -> Value
encodePScene val =
    Point2d.toUnitless val |> encodeXy


rSceneDecoder : Decoder RScene
rSceneDecoder =
    Decode.succeed (\x y w h -> xywhToRect { x = x, y = y, w = w, h = h })
        |> DE.andMap (Decode.field "x" Decode.float)
        |> DE.andMap (Decode.field "y" Decode.float)
        |> DE.andMap (Decode.field "w" Decode.float)
        |> DE.andMap (Decode.field "h" Decode.float)


encodeRScene : RScene -> Value
encodeRScene val =
    let
        { x, y, w, h } =
            rectToXywh val
    in
    Encode.object
        [ ( "x", Encode.float x )
        , ( "y", Encode.float y )
        , ( "w", Encode.float w )
        , ( "h", Encode.float h )
        ]


rectToXywh : Rectangle2d Unitless coord -> { x : Float, y : Float, w : Float, h : Float }
rectToXywh rect =
    let
        ( w, h ) =
            Rectangle2d.dimensions rect
                |> Tuple.mapBoth Quantity.toFloat Quantity.toFloat

        { x, y } =
            Rectangle2d.interpolate rect 0 0
                |> Point2d.toUnitless
    in
    { x = x
    , y = y
    , w = w
    , h = h
    }


xywhToRect : { x : Float, y : Float, w : Float, h : Float } -> Rectangle2d Unitless coord
xywhToRect { x, y, w, h } =
    Rectangle2d.from
        (Point2d.unitless x y)
        (Point2d.unitless (w + x) (h + y))


rectDeltaSize : Vector2d Unitless coord -> Rectangle2d Unitless coord -> Rectangle2d Unitless coord
rectDeltaSize vect rect =
    let
        { x, y } =
            Vector2d.toUnitless vect
    in
    rectToXywh rect
        |> (\xywh ->
                { xywh
                    | w = x + xywh.w |> max 0
                    , h = y + xywh.h |> max 0
                }
           )
        |> xywhToRect


rectSetSize : Vector2d Unitless coord -> Rectangle2d Unitless coord -> Rectangle2d Unitless coord
rectSetSize vect rect =
    let
        { x, y } =
            Vector2d.toUnitless vect
    in
    rectToXywh rect
        |> (\xywh ->
                { xywh
                    | w = x |> max 0
                    , h = y |> max 0
                }
           )
        |> xywhToRect


minimumRectSize : Vector2d Unitless coord -> Rectangle2d Unitless coord -> Rectangle2d Unitless coord
minimumRectSize size rect =
    let
        { x, y, w, h } =
            rectToXywh rect

        sizeXy =
            Vector2d.toUnitless size

        minW =
            max w sizeXy.x

        minH =
            max h sizeXy.y
    in
    xywhToRect { x = x, y = y, w = minW, h = minH }


fitRectToMaxSize : Vector2d Unitless coord -> Rectangle2d Unitless coord -> Rectangle2d Unitless coord
fitRectToMaxSize size rect =
    let
        { x, y, w, h } =
            rectToXywh rect

        sizeXy =
            Vector2d.toUnitless size

        wScaleFactor =
            if w > sizeXy.x then
                sizeXy.x / w

            else
                1.0

        hScaleFactor =
            if h > sizeXy.y then
                sizeXy.y / h

            else
                1.0

        scaleFactor =
            min 1.0 wScaleFactor
                |> min hScaleFactor
    in
    xywhToRect { x = x, y = y, w = w * scaleFactor, h = h * scaleFactor }


rectToXywhPixels : Rectangle2d Pixels coord -> { x : Float, y : Float, w : Float, h : Float }
rectToXywhPixels rect =
    let
        ( w, h ) =
            Rectangle2d.dimensions rect
                |> Tuple.mapBoth Pixels.toFloat Pixels.toFloat

        { x, y } =
            Rectangle2d.interpolate rect 0 0
                |> Point2d.toPixels
    in
    { x = x
    , y = y
    , w = w
    , h = h
    }


bboxToRect : BoundingBox2d units coord -> Rectangle2d units coord
bboxToRect bbox =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema bbox
    in
    Rectangle2d.with { x1 = minX, y1 = minY, x2 = maxX, y2 = maxY }


{-| Using the specified point within the scene as the origin of a local
coordinate system for the bounding box, translate the bounding box into
the scene.
-}
bboxToScene : PScene -> BLocal -> BScene
bboxToScene pos bbox =
    let
        ( originX, originY ) =
            Point2d.toTuple Quantity.toFloat pos

        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema bbox
    in
    { minX = Quantity.toFloat minX + originX |> Quantity.float
    , minY = Quantity.toFloat minY + originY |> Quantity.float
    , maxX = Quantity.toFloat maxX + originX |> Quantity.float
    , maxY = Quantity.toFloat maxY + originY |> Quantity.float
    }
        |> BoundingBox2d.fromExtrema


{-| Using the specified point within the scene as the origin of a local
coordinate system for the bounding box, translate a bounding box into
local coordinates.
-}
bboxToLocal : PScene -> BScene -> BLocal
bboxToLocal pos bbox =
    let
        ( originX, originY ) =
            Point2d.toTuple Quantity.toFloat pos

        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema bbox
    in
    { minX = Quantity.toFloat minX - originX |> Quantity.float
    , minY = Quantity.toFloat minY - originY |> Quantity.float
    , maxX = Quantity.toFloat maxX - originX |> Quantity.float
    , maxY = Quantity.toFloat maxY - originY |> Quantity.float
    }
        |> BoundingBox2d.fromExtrema


{-| Given a point in the scene, supplies the vector from that point to the origin of the scene.

Useful for deriving local coordinates within elements of the scene.

-}
pointToSceneOrigin : PScene -> VScene
pointToSceneOrigin pos =
    Vector2d.from pos Point2d.origin
