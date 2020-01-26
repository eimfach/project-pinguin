module AssetsSmall exposing (deepOcean, genericForest, genericLake, genericLandmass, mixedPlane, pod)

-- deprecated

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten, saturate)
import Colors
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy
import World


mixedPlane : { gridColor : Maybe Color } -> Svg msg
mixedPlane { gridColor } =
    create "mixed-plane" gridColor Colors.basicLandMassBackgroundColor mixedPlaneObject


genericForest : { gridColor : Maybe Color } -> World.Chunk -> Svg msg
genericForest { gridColor } chunk =
    let
        landMassColor =
            case chunk.ecoSystemType of
                World.ModerateEcoSystemType ->
                    Colors.basicLandMassBackgroundColor

                World.MoonEcoSystemType ->
                    Colors.moonLandMassBackgroundColor

                _ ->
                    Colors.invalidColor
    in
    create
        (World.coordinatesToString <| World.unwrapWorldSpace chunk.location)
        gridColor
        landMassColor
        -- place tree position, color variations, amount/density (growth and age) randomly
        (g [] (List.map (mapTreeData chunk) chunk.layers.ground.objects.trees))


genericLake : { gridColor : Maybe Color } -> Svg msg
genericLake { gridColor } =
    create "generic-lake" gridColor Colors.basicLakeBackgroundColor lakeObject


deepOcean : { gridColor : Maybe Color } -> Svg msg
deepOcean { gridColor } =
    create "deep-ocean" gridColor Colors.basicDeepOceanBackgroundColor oceanObject


genericLandmass : { gridColor : Maybe Color } -> Svg msg
genericLandmass { gridColor } =
    create "generic-landmass" gridColor Colors.basicLandMassBackgroundColor (g [] [])


pod : { gridColor : Maybe Color } -> Svg msg
pod { gridColor } =
    create "pod" gridColor Color.black (g [] [])


hexPoints =
    "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"


{-|

    The main function for creating an svg asset

-}
create : String -> Maybe Color -> Color -> Svg msg -> Svg msg
create id_ gridColor fillColor el =
    let
        translatedGridColor =
            case gridColor of
                Just aColor ->
                    colorToHex aColor

                Nothing ->
                    "transparent"
    in
    g [ id id_ ]
        [ polygon
            [ stroke translatedGridColor
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
                            Colors.moderateMagicTreeBaseColor

                        _ ->
                            Colors.moderateTreeBaseColor

                World.MoonEcoSystemType ->
                    Colors.moonTreeBaseColor

                _ ->
                    Colors.invalidColor

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



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
---------------------------------------------- OBJECTS -------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


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
            , fill <| colorToHex Colors.moderateTreeStemBaseColor
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
            [ fill <| colorToHex Colors.wavesLakeColor
            , fillOpacity "0.5"
            , x "-1"
            , y "-2"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex Colors.wavesLakeColor
            , fillOpacity "0.75"
            , x "2"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex Colors.wavesLakeColor
            , fillOpacity "0.75"
            , x "-3"
            , y "2"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex Colors.wavesLakeColor
            , fillOpacity "0.5"
            , x "-4"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex Colors.wavesLakeColor
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
            [ fill <| colorToHex <| lighten 0.1 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "2"
            , y "4"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.1 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "0"
            , y "-7"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.05 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "0"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| lighten 0.1 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-6"
            , y "-2"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| lighten 0.05 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "5"
            , y "0"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.05 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-3"
            , y "-4"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.15 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "2"
            , y "-2"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| lighten 0.15 Colors.mixedPlaneObjectsColor
            , fillOpacity "0.5"
            , x "-2"
            , y "-1"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill <| colorToHex <| darken 0.2 Colors.mixedPlaneObjectsColor
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
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-5"
            , y "-7"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "0"
            , y "-7"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "4"
            , y "-7"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "4"
            , y "-4"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-3"
            , y "-4"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "0"
            , y "-5"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-5"
            , y "-2"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-8"
            , y "0"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-7"
            , y "2"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-3"
            , y "1"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-5"
            , y "5"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "4"
            , y "2"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "0"
            , y "3"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "1"
            , y "6"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "3"
            , y "4"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-4"
            , y "7"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "0"
            , y "-2"
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "3"
            , y "-1"
            , width "3"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "7"
            , y "1"
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill Colors.deepOceanObjectColorAsHex
            , x "-8"
            , y "-4"
            , width "3"
            , height "1"
            ]
            []
        ]
