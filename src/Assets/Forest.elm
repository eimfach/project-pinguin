module Assets.Forest exposing (genericForest)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten, saturate)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy
import World


moderateTreeBaseColor =
    Color.rgb255 65 117 5


moderateMagicTreeColor =
    --Color.rgb255 89 117 5
    Color.rgb255 6 148 108


moonTreeBaseColor =
    Color.rgb255 36 5 117


invalidColor =
    Color.red


genericForest : World.Chunk -> Svg msg
genericForest chunk =
    g [ id <| World.coordinatesToString chunk.coordinate ]
        (List.append
            [ polygon
                [ stroke "transparent"
                , strokeWidth "0.5"
                , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
                ]
                []

            -- place tree position, color variations, amount/density (growth and age) randomly
            ]
            (List.map (mapTreeData chunk) chunk.layers.ground.objects.trees)
        )


mapTreeData chunk treeInstance =
    let
        baseColor =
            case chunk.ecoSystemType of
                World.ModerateEcoSystemType ->
                    case chunk.biome of
                        World.Forest World.MagicForest _ _ _ ->
                            moderateMagicTreeColor

                        _ ->
                            moderateTreeBaseColor

                World.MoonEcoSystemType ->
                    moonTreeBaseColor

                _ ->
                    invalidColor

        color =
            case treeInstance.treeType of
                World.MixedForestDefault ->
                    colorToHex <| baseColor

                World.MixedForestDark ->
                    colorToHex <| darken 0.05 baseColor

                World.MixedForestLight ->
                    colorToHex <| lighten 0.075 baseColor
    in
    Svg.Lazy.lazy
        tree
        ( color, { nativeX = treeInstance.coordinate.x, nativeY = treeInstance.coordinate.y } )


tree : ( String, { nativeX : Int, nativeY : Int } ) -> Svg msg
tree ( color, { nativeX, nativeY } ) =
    g []
        [ rect
            [ fillOpacity "0.75"
            , fill color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill color
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill color
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY + 0
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.75"
            , fill color
            , x <| String.fromInt <| nativeX + 0
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []

        -- outside leaves
        , rect
            [ fillOpacity "0.5"
            , fill color
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill color
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill color
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity "0.5"
            , fill color
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        ]
