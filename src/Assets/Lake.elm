module Assets.Lake exposing (generic)

import Color
import Color.Convert exposing (colorToHex)
import Svg exposing (..)
import Svg.Attributes exposing (..)


baseColor =
    Color.rgb255 31 180 255


wavesColor =
    Color.rgb255 33 222 255


generic : Svg msg
generic =
    g [ id "generic-lake" ]
        [ polygon
            [ stroke "transparent"
            , strokeWidth "0.5"
            , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
            ]
            []
        , g
            []
            [ rect
                [ fill <| colorToHex wavesColor
                , fillOpacity "0.5"
                , x "-1"
                , y "-2"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex wavesColor
                , fillOpacity "0.75"
                , x "2"
                , y "0"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex wavesColor
                , fillOpacity "0.75"
                , x "-3"
                , y "2"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex wavesColor
                , fillOpacity "0.5"
                , x "-4"
                , y "0"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex wavesColor
                , fillOpacity "0.75"
                , x "-2"
                , y "-5"
                , width "1"
                , height "1"
                ]
                []
            ]
        ]
