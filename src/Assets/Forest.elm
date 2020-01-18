module Assets.Forest exposing (genericForest)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import World


mixedTreeColor =
    -- convert to rgb
    Color.rgb255 65 117 5


genericForest : World.Chunk -> Svg msg
genericForest chunk =
    g [ id <| World.coordinatesToString chunk.coordinate ]
        (List.append
            [ polygon
                [ stroke "#000000"
                , strokeWidth "0.5"
                , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
                ]
                []

            -- place tree position, color variations, amount/density (growth and age) randomly
            ]
            (List.map mapTreeData chunk.layers.ground.objects.trees)
        )


mapTreeData treeInstance =
    let
        color =
            case treeInstance.treeType of
                World.MixedForestDefault ->
                    mixedTreeColor

                World.MixedForestDark ->
                    darken 0.075 mixedTreeColor

                World.MixedForestLight ->
                    lighten 0.075 mixedTreeColor
    in
    tree { nativeX = treeInstance.coordinate.x, nativeY = treeInstance.coordinate.y } color


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
