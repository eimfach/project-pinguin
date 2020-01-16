module Assets.Basic exposing (generic)

import Svg exposing (..)
import Svg.Attributes exposing (..)


generic : Svg msg
generic =
    g [ id "pod" ]
        [ polygon
            [ stroke "#000000"
            , strokeWidth "0.5"
            , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
            ]
            []
        ]
