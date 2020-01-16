module Assets.Forest exposing (mixedForest)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten)
import Svg exposing (..)
import Svg.Attributes exposing (..)


mixedTreeColor =
    Color.Convert.hexToColor "417505"


mixedForest =
    case mixedTreeColor of
        Ok color ->
            g [ id "mixed-forest" ]
                [ polygon
                    [ stroke "#000000"
                    , strokeWidth "0.5"
                    , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
                    ]
                    []

                -- place tree position, color variations, amount/density (growth and age) randomly
                , tree { hexX = 0, hexY = 0 } <| darken 0.1 color
                , tree { hexX = 5, hexY = -2 } <| darken 0.15 color
                , tree { hexX = 2, hexY = 7 } <| lighten 0.3 color
                , tree { hexX = 10, hexY = 2 } <| lighten 0.2 color
                , tree { hexX = 1, hexY = 6 } color
                , tree { hexX = 3, hexY = 1 } <| lighten 0.05 color
                , tree { hexX = 4, hexY = 5 } color
                , tree { hexX = 4, hexY = -7 } <| darken 0.1 color
                , tree { hexX = -4, hexY = -3 } color
                , tree { hexX = -12, hexY = -1 } <| darken 0.15 color
                , tree { hexX = 8, hexY = -4 } color
                , tree { hexX = 1, hexY = -9 } <| lighten 0.18 color
                , tree { hexX = -4, hexY = -2 } <| lighten 0.11 color
                , tree { hexX = 4, hexY = 8 } <| darken 0.05 color
                , tree { hexX = -4, hexY = 8 } color
                , tree { hexX = -1, hexY = -10 } <| darken 0.1 color
                , tree { hexX = -8, hexY = 8 } color
                ]

        Err err ->
            g [] []


tree : { hexX : Int, hexY : Int } -> Color -> Svg msg
tree { hexX, hexY } color =
    g []
        [ rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 0
            , y <| String.fromInt <| hexY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 1
            , y <| String.fromInt <| hexY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 0
            , y <| String.fromInt <| hexY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX - 1
            , y <| String.fromInt <| hexY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 0
            , y <| String.fromInt <| hexY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 0
            , y <| String.fromInt <| hexY - 1
            , width "1"
            , height "1"
            ]
            []

        -- outside leaves
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 1
            , y <| String.fromInt <| hexY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + 1
            , y <| String.fromInt <| hexY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX + -1
            , y <| String.fromInt <| hexY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| hexX - 1
            , y <| String.fromInt <| hexY - 1
            , width "1"
            , height "1"
            ]
            []
        ]
