module Assets exposing (deepOcean, genericForest, genericLake, genericLandmass, mixedPlane, pod)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten, saturate)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy
import World


mixedPlane : Svg msg
mixedPlane =
    create "mixed-plane" basicLandMassBackgroundColor mixedPlaneObject


genericForest : World.Chunk -> Svg msg
genericForest chunk =
    create
        (World.coordinatesToString chunk.coordinate)
        basicLandMassBackgroundColor
        -- place tree position, color variations, amount/density (growth and age) randomly
        (g [] (List.map (mapTreeData chunk) chunk.layers.ground.objects.trees))


genericLake : Svg msg
genericLake =
    create "generic-lake" basicLakeBackgroundColor lakeObject


deepOcean : Svg msg
deepOcean =
    create "deep-ocean" basicDeepOceanBackgroundColor oceanObject


genericLandmass : Svg msg
genericLandmass =
    create "generic-landmass" basicLandMassBackgroundColor (g [] [])


pod : Svg msg
pod =
    create "pod" Color.black (g [] [])


hexPoints =
    "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"


basicDeepOceanBackgroundColor =
    Color.rgb255 50 108 149


basicLandMassBackgroundColor =
    Color.rgb255 66 135 66


basicLakeBackgroundColor =
    Color.rgb255 31 180 255


moderateTreeStemBaseColor =
    Color.rgb255 114 90 69


moderateTreeBaseColor =
    Color.rgb255 65 117 5


moderateMagicTreeColor =
    --Color.rgb255 89 117 5
    Color.rgb255 6 148 108


moonTreeBaseColor =
    Color.rgb255 36 5 117


invalidColor =
    Color.red


baseLakeColor =
    Color.rgb255 31 180 255


wavesLakeColor =
    Color.rgb255 33 222 255


deepOceanObjectColor =
    "#0098B2"


mixedPlaneObjectsColor =
    Color.rgb255 66 135 100


create : String -> Color -> Svg msg -> Svg msg
create id_ fillColor el =
    g [ id id_ ]
        [ polygon
            [ stroke "transparent"
            , fill <| colorToHex fillColor
            , strokeWidth "0.5"
            , points hexPoints
            ]
            []
        , el
        ]


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
        treeObject
        ( color, { nativeX = treeInstance.coordinate.x, nativeY = treeInstance.coordinate.y } )


treeObject : ( String, { nativeX : Int, nativeY : Int } ) -> Svg msg
treeObject ( color, { nativeX, nativeY } ) =
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

        -- stem
        , rect
            [ fillOpacity "0.4"
            , fill <| colorToHex moderateTreeStemBaseColor
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY + 2
            , width "1"
            , height "1"
            ]
            []
        ]


lakeObject =
    g
        []
        [ rect
            [ fill <| colorToHex wavesLakeColor
            , fillOpacity "0.5"
            , x "-1"
            , y "-2"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex wavesLakeColor
            , fillOpacity "0.75"
            , x "2"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex wavesLakeColor
            , fillOpacity "0.75"
            , x "-3"
            , y "2"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex wavesLakeColor
            , fillOpacity "0.5"
            , x "-4"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex wavesLakeColor
            , fillOpacity "0.75"
            , x "-2"
            , y "-5"
            , width "1"
            , height "1"
            ]
            []
        ]


mixedPlaneObject =
    g []
        [ rect
            [ fill <| colorToHex <| lighten 0.1 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "2"
            , y "4"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.1 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "0"
            , y "-7"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.05 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "0"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| lighten 0.1 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-6"
            , y "-2"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| lighten 0.05 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "5"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.05 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-3"
            , y "-4"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.15 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "2"
            , y "-2"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| lighten 0.15 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-2"
            , y "-1"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.2 mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-3"
            , y "4"
            , width "2"
            , height "1"
            ]
            []
        ]


oceanObject =
    g
        []
        [ rect
            [ fill deepOceanObjectColor
            , x "-5"
            , y "-7"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "0"
            , y "-7"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "4"
            , y "-7"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "4"
            , y "-4"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-3"
            , y "-4"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "0"
            , y "-5"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-5"
            , y "-2"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-8"
            , y "0"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-7"
            , y "2"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-3"
            , y "1"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-5"
            , y "5"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "4"
            , y "2"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "0"
            , y "3"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "1"
            , y "6"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "3"
            , y "4"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-4"
            , y "7"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "0"
            , y "-2"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "3"
            , y "-1"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "7"
            , y "1"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill deepOceanObjectColor
            , x "-8"
            , y "-4"
            , width "3"
            , height "1"
            ]
            []
        ]
