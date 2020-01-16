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
                , tree { nativeX = 0, nativeY = 0 } <| darken 0.1 color
                , tree { nativeX = 5, nativeY = -2 } <| darken 0.15 color
                , tree { nativeX = 2, nativeY = 7 } <| lighten 0.3 color
                , tree { nativeX = 10, nativeY = 2 } <| lighten 0.2 color
                , tree { nativeX = 1, nativeY = 6 } color
                , tree { nativeX = 3, nativeY = 1 } <| lighten 0.05 color
                , tree { nativeX = 4, nativeY = 5 } color
                , tree { nativeX = 4, nativeY = -7 } <| darken 0.1 color
                , tree { nativeX = -4, nativeY = -3 } color
                , tree { nativeX = -12, nativeY = -1 } <| darken 0.15 color
                , tree { nativeX = 8, nativeY = -4 } color
                , tree { nativeX = 1, nativeY = -9 } <| lighten 0.18 color
                , tree { nativeX = -4, nativeY = -2 } <| lighten 0.11 color
                , tree { nativeX = 4, nativeY = 8 } <| darken 0.05 color
                , tree { nativeX = -4, nativeY = 8 } color
                , tree { nativeX = -1, nativeY = -10 } <| darken 0.1 color
                , tree { nativeX = -8, nativeY = 8 } color
                ]

        Err err ->
            g [] []


tree : { nativeX : Int, nativeY : Int } -> Color -> Svg msg
tree { nativeX, nativeY } color =
    g []
        [ rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []

        -- outside leaves
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX + -1
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill <| colorToHex color
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        ]
