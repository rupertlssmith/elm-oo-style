module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Config
import Css.Global
import Geometry exposing (VScreen)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Lazy
import Html.Styled as HS
import Scene.Drawing as Drawing
import Scene.Root as Root
import Scene.Spec exposing (Msg(..), Scene(..))
import Scene.Target as Target
import Style
import Task
import Update2 as U2
import Vector2d


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
    | Ready Scene


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindow
    , Task.perform (viewportToSize >> WindowSize) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Browser.Events.onResize coordsToSize |> Sub.map WindowSize
    , case model of
        Ready (Scene scene) ->
            scene.subscriptions

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



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --case ( model, Debug.log "msg" msg ) of
    case ( model, msg ) of
        ( SizingWindow, WindowSize windowSize ) ->
            let
                drawing =
                    Drawing.empty windowSize "svg-drawing"
                        |> Drawing.add "root" (Root.root "root")
                        |> Drawing.add "target" (Target.target "target")
            in
            U2.pure
                (Ready drawing)

        ( Ready (Scene scene), _ ) ->
            scene.update msg |> Tuple.mapFirst Ready

        _ ->
            U2.pure model



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "SVG Drawing Example"
    , body =
        [ Style.global Config.config |> Css.Global.global |> HS.toUnstyled
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
        Ready (Scene scene) ->
            H.div
                [ HA.id "top-container"
                ]
                [ leftMenu
                , scene.view ()
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
