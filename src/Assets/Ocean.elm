module Assets.Ocean exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten, saturate)
import Svg exposing (..)
import Svg.Attributes exposing (..)


deepOceanColor =
    "#0098B2"


deepOcean : Svg msg
deepOcean =
    g [ id "deep-ocean" ]
        [ polygon
            [ stroke "transparent"
            , strokeWidth "0.5"
            , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
            ]
            []
        , g
            []
            [ rect
                [ fill deepOceanColor
                , x "-5"
                , y "-7"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "0"
                , y "-7"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "4"
                , y "-7"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "4"
                , y "-4"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-3"
                , y "-4"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "0"
                , y "-5"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-5"
                , y "-2"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-8"
                , y "0"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-7"
                , y "2"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-3"
                , y "1"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-5"
                , y "5"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "4"
                , y "2"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "0"
                , y "3"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "1"
                , y "6"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "3"
                , y "4"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-4"
                , y "7"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "0"
                , y "-2"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "3"
                , y "-1"
                , width "3"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "7"
                , y "1"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill deepOceanColor
                , x "-8"
                , y "-4"
                , width "3"
                , height "1"
                ]
                []
            ]
        ]
