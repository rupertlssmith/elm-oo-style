module Style exposing (..)

import Color
import Css
import Css.Global


printBlack =
    Css.rgb 25 25 25


offWhite =
    Color.rgb255 248 240 245


type alias Config a =
    { a
        | leftMenuWidth : Float
        , rightOverlayWidth : Float
    }


global : Config a -> List Css.Global.Snippet
global config =
    [ Css.Global.html
        [ Css.pct 100 |> Css.height ]
    , Css.Global.body
        [ Css.pct 100 |> Css.height
        , Css.overflow Css.hidden
        , Css.touchAction Css.none

        -- Required for Chrome or you get pointercancel when drags start and end on different divs.
        , Css.property "user-select" "none"
        ]
    , Css.Global.id "top-container"
        [ Css.pct 100 |> Css.width
        , Css.pct 100 |> Css.height
        , Css.overflow Css.hidden
        , Css.displayFlex
        , Css.flexDirection Css.row
        , Css.justifyContent Css.flexStart
        , Css.alignItems Css.stretch
        ]
    , Css.Global.id "drawing-container"
        [ Css.pct 100 |> Css.width
        , Css.pct 100 |> Css.height

        -- , Css.overflow Css.hidden
        -- , Css.displayFlex
        -- , Css.flexDirection Css.row
        -- , Css.justifyContent Css.flexStart
        -- , Css.alignItems Css.stretch
        ]
    , Css.Global.id "svg-drawing"
        []
    , Css.Global.id "params-container"
        [ Css.rgb 250 250 250 |> Css.backgroundColor
        , Css.position Css.fixed
        , Css.border3 (Css.px 3) Css.solid printBlack
        , Css.borderRadius (Css.px 5)
        , Css.padding2 (Css.px 20) (Css.px 20)
        , Css.margin2 (Css.px 10) (Css.px 10)
        , Css.left (Css.px 0)
        , Css.top (Css.px 0)
        , Css.zIndex (Css.int 2000)
        ]
    , Css.Global.id "left-menu"
        [ Css.rgb 25 25 25 |> Css.backgroundColor
        , Css.px config.leftMenuWidth |> Css.width
        ]
    , Css.Global.id "right-overlay"
        [ Css.position Css.fixed
        , Css.px 0 |> Css.top
        , Css.px 0 |> Css.right
        , Css.px config.rightOverlayWidth |> Css.minWidth
        , Css.px config.rightOverlayWidth |> Css.maxWidth
        , Css.vh 100 |> Css.height
        , Css.rgba 25 25 25 0.5 |> Css.backgroundColor
        ]
    , Css.Global.selector "h3"
        [ Css.px 6 |> Css.marginTop
        , Css.px 10 |> Css.marginBottom
        ]
    , Css.Global.class "noselect"
        [ Css.property "user-select" "none"
        , Css.property "-moz-user-select" "none"
        , Css.property "-webkit-user-select" "none"
        , Css.property "-ms-user-select" "none"
        ]
    , Css.Global.selector "::selection"
        [ Css.backgroundColor (Css.rgb 196 195 217)
        ]
    ]
