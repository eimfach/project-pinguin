module Assets.Plane exposing (..)

import Color
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten, saturate)
import Svg exposing (..)
import Svg.Attributes exposing (..)


floorColor =
    Color.rgb255 66 135 100


mixedPlane : Svg msg
mixedPlane =
    g [ id "mixed-plane" ]
        [ polygon
            [ stroke "transparent"
            , strokeWidth "0.25"
            , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
            ]
            []
        , g []
            [ rect
                [ fill <| colorToHex <| lighten 0.1 floorColor
                , fillOpacity "0.5"
                , x "2"
                , y "4"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| darken 0.1 floorColor
                , fillOpacity "0.5"
                , x "0"
                , y "-7"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| darken 0.05 floorColor
                , fillOpacity "0.5"
                , x "0"
                , y "0"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| lighten 0.1 floorColor
                , fillOpacity "0.5"
                , x "-6"
                , y "-2"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| lighten 0.05 floorColor
                , fillOpacity "0.5"
                , x "5"
                , y "0"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| darken 0.05 floorColor
                , fillOpacity "0.5"
                , x "-3"
                , y "-4"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| darken 0.15 floorColor
                , fillOpacity "0.5"
                , x "2"
                , y "-2"
                , width "1"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| lighten 0.15 floorColor
                , fillOpacity "0.5"
                , x "-2"
                , y "-1"
                , width "2"
                , height "1"
                ]
                []
            , rect
                [ fill <| colorToHex <| darken 0.2 floorColor
                , fillOpacity "0.5"
                , x "-3"
                , y "4"
                , width "2"
                , height "1"
                ]
                []
            ]
        ]
