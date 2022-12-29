module Config exposing (..)

import Array
import Color
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
    , containerElementId = "root"
    , minimumNoteSize = Vector2d.unitless 60 60
    , editorId = "sticky-1"
    , leftMenuWidth = 50
    , rightOverlayWidth = 250
    }
