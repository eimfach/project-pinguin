module Assets exposing
    ( deepOcean
    , genericForest
    , genericLake
    , genericLandmass
    , leaveTreeObject
    , magicalTreeObject
    , mixedPlane
    , pickForestColor
    , pod
    )

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

                World.OceanEcoSystemType ->
                    Colors.invalidColor
    in
    create
        (World.coordinatesToString chunk.coordinate)
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
    "30 0 10 0 0 18 10 36 30 36 40 18"


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
            , fillRule "nonzero"
            , strokeWidth "0.5"
            , points hexPoints
            ]
            []
        , el
        ]


pickForestColor : World.Chunk -> ( Color, Color )
pickForestColor chunk =
    case chunk.ecoSystemType of
        World.ModerateEcoSystemType ->
            case chunk.biome of
                World.Forest World.MagicForest _ _ _ ->
                    ( Colors.moderateMagicTreeBaseColor, Colors.moderateMagicTreeLeavesColor )

                _ ->
                    ( Colors.moderateTreeBaseColor, Colors.moderateTreeLeavesColor )

        World.MoonEcoSystemType ->
            case chunk.biome of
                World.Forest World.MagicForest _ _ _ ->
                    ( Colors.moonMagicTreeBaseColor, Colors.moonMagicTreeLeavesColor )

                _ ->
                    ( Colors.moonTreeBaseColor, Colors.moonTreeLeavesColor )

        _ ->
            ( Colors.invalidColor, darken 0.02 Colors.invalidColor )


mapTreeData chunk treeInstance =
    let
        ( baseColor, leavesColor ) =
            pickForestColor chunk

        ( baseColorAsHex, leavesColorAsHex ) =
            case treeInstance.treeType of
                World.MixedForestDefault ->
                    ( colorToHex <| baseColor, colorToHex <| leavesColor )

                World.MixedForestDark ->
                    ( colorToHex <| darken 0.05 baseColor, colorToHex <| darken 0.05 leavesColor )

                World.MixedForestLight ->
                    ( colorToHex <| lighten 0.05 baseColor, colorToHex <| lighten 0.05 leavesColor )

        coordinates =
            { nativeX = treeInstance.coordinate.x, nativeY = treeInstance.coordinate.y }
    in
    case chunk.biome of
        World.Forest World.MagicForest _ _ _ ->
            Svg.Lazy.lazy
                leaveTreeObject
                ( { colorBase = baseColorAsHex, colorLeaves = leavesColorAsHex }
                , coordinates
                )

        _ ->
            Svg.Lazy.lazy
                leaveTreeObject
                ( { colorBase = baseColorAsHex, colorLeaves = leavesColorAsHex }
                , coordinates
                )



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
---------------------------------------------- OBJECTS -------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


magicalTreeObject : { nativeX : Int, nativeY : Int } -> Svg msg
magicalTreeObject { nativeX, nativeY } =
    g []
        -- 2,4
        [ rect
            [ fill "#725A45"
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#7D624B"
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#C6AA01"
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#C6AA01"
            , x <| String.fromInt <| nativeX - 2
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#968101"
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#968101"
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#968101"
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 3
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#968101"
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#C6AA01"
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 4
            , width "1"
            , height "3"
            ]
            []
        , rect
            [ fill "#C6AA01"
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY - 4
            , width "1"
            , height "3"
            ]
            []
        , rect
            [ fill "#C6AA01"
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#C6AA01"
            , x <| String.fromInt <| nativeX + 2
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#968101"
            , x <| String.fromInt <| nativeX + 2
            , y <| String.fromInt <| nativeY - 2
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#968101"
            , x <| String.fromInt <| nativeX - 2
            , y <| String.fromInt <| nativeY - 2
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fillOpacity
                "0.5"
            , fill "#05A3DF"
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 2
            , width "1"
            , height "1"
            ]
            []
        ]


leaveTreeObject : ( { colorBase : String, colorLeaves : String }, { nativeX : Int, nativeY : Int } ) -> Svg msg
leaveTreeObject ( { colorBase, colorLeaves }, { nativeX, nativeY } ) =
    g []
        -- 16 15
        [ rect
            [ fill "#725A45"
            , fillOpacity "0.75"
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill "#7D624B"
            , fillOpacity "0.75"
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY + 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX + 3
            , y <| String.fromInt <| nativeY - 3
            , width "1"
            , height "2"
            ]
            []
        , rect
            [ fillOpacity "0.05"
            , fill colorBase
            , x <| String.fromInt <| nativeX + 2
            , y <| String.fromInt <| nativeY - 4
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 3
            , width "1"
            , height "2"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 1
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 3
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 4
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorLeaves
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 4
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorLeaves
            , x <| String.fromInt <| nativeX
            , y <| String.fromInt <| nativeY - 2
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorLeaves
            , x <| String.fromInt <| nativeX + 2
            , y <| String.fromInt <| nativeY - 3
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorLeaves
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY - 2
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorLeaves
            , x <| String.fromInt <| nativeX + 2
            , y <| String.fromInt <| nativeY - 1
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorLeaves
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 2
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX - 1
            , y <| String.fromInt <| nativeY - 3
            , width "1"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX + 1
            , y <| String.fromInt <| nativeY - 4
            , width "2"
            , height "1"
            ]
            []
        , rect
            [ fill colorBase
            , x <| String.fromInt <| nativeX + 2
            , y <| String.fromInt <| nativeY - 2
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
        [ rect [ fillOpacity "0.5", fill "#FF2E47", x "29", y "26", width "1", height "1" ] []
        , rect [ fill "#58B258", x "22", y "12", width "1", height "1" ] []
        , rect [ fill "#58B258", x "22", y "12", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "23", y "11", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "6", y "23", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "10", y "27", width "1", height "2" ] []
        , rect [ fill "#58B258", x "11", y "26", width "1", height "1" ] []
        , rect [ fill "#58B258", x "26", y "23", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "20", y "33", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "34", y "13", width "1", height "1" ] []
        , rect [ fill "#58B258", x "35", y "18", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "36", y "16", width "1", height "2" ] []
        , rect [ fill "#4D9E4D", x "10", y "10", width "2", height "1" ] []
        , rect [ fill "#4D9E4D", x "9", y "9", width "1", height "1" ] []
        , rect [ fill "#58B258", x "11", y "11", width "1", height "1" ] []
        , rect [ fill "#58B258", x "12", y "9", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "23", y "17", width "1", height "2" ] []
        , rect [ fill "#387438", x "24", y "17", width "1", height "1" ] []
        , rect [ fill "#387438", x "25", y "16", width "1", height "1" ] []
        , rect [ fill "#387438", x "30", y "16", width "1", height "1" ] []
        , rect [ fill "#387438", x "15", y "26", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "25", y "6", width "1", height "1" ] []
        , rect [ fill "#387438", x "16", y "12", width "1", height "1" ] []
        , rect [ fill "#387438", x "12", y "4", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "25", y "31", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "17", y "8", width "1", height "2" ] []
        , rect [ fill "#387438", x "15", y "7", width "2", height "1" ] []
        , rect [ fill "#387438", x "18", y "8", width "1", height "1" ] []
        , rect [ fill "#387438", x "19", y "7", width "1", height "1" ] []
        , rect [ fillOpacity "0.61888587", fill "#F6A623", x "17", y "7", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "15", y "22", width "1", height "2" ] []
        , rect [ fill "#4D9E4D", x "28", y "28", width "1", height "1" ] []
        , rect [ fill "#58B258", x "27", y "27", width "1", height "1" ] []
        , rect [ fill "#58B258", x "29", y "27", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "30", y "26", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "13", y "21", width "1", height "1" ] []
        , rect [ fill "#58B258", x "14", y "21", width "1", height "1" ] []
        , rect [ fill "#58B258", x "16", y "22", width "1", height "1" ] []
        , rect [ fill "#58B258", x "17", y "21", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "6", y "17", width "1", height "2" ] []
        , rect [ fill "#387438", x "7", y "17", width "1", height "1" ] []
        , rect [ fill "#387438", x "8", y "16", width "1", height "1" ] []
        , rect [ fillOpacity "0.5", fill "#FF2E47", x "8", y "17", width "1", height "1" ] []
        , rect [ fillOpacity "0.5", fill "#FF2E47", x "29", y "9", width "1", height "1" ] []
        , rect [ fill "#387438", x "9", y "15", width "1", height "1" ] []
        , rect [ fill "#387438", x "5", y "16", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "29", y "10", width "1", height "2" ] []
        , rect [ fill "#5BB75B", x "28", y "9", width "1", height "1" ] []
        , rect [ fill "#58B258", x "30", y "10", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "31", y "9", width "1", height "1" ] []
        , rect [ fill "#387438", x "30", y "22", width "1", height "2" ] []
        , rect [ fill "#2C5C2C", x "29", y "21", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "31", y "22", width "1", height "1" ] []
        , rect [ fill "#387438", x "32", y "21", width "1", height "1" ] []
        , rect [ fill "#2C5C2C", x "22", y "27", width "1", height "2" ] []
        , rect [ fill "#387438", x "20", y "26", width "2", height "1" ] []
        , rect [ fill "#387438", x "23", y "27", width "1", height "1" ] []
        , rect [ fill "#387438", x "15", y "31", width "1", height "2" ] []
        , rect [ fill "#2C5C2C", x "13", y "30", width "2", height "1" ] []
        , rect [ fill "#387438", x "16", y "31", width "1", height "1" ] []
        , rect [ fill "#387438", x "17", y "30", width "1", height "1" ] []
        , rect [ fill "#58B258", x "17", y "16", width "1", height "2" ] []
        , rect [ fill "#4D9E4D", x "16", y "15", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "22", y "3", width "1", height "1" ] []
        , rect [ fill "#58B258", x "21", y "2", width "1", height "1" ] []
        , rect [ fill "#58B258", x "23", y "2", width "1", height "1" ] []
        , rect [ fill "#4D9E4D", x "24", y "1", width "1", height "1" ] []
        ]


oceanObject =
    g []
        [ rect [ fill Colors.deepOceanObjectColorAsHex, x "6", y "9", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "15", y "9", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "19", y "9", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "17", y "12", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "11", y "6", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "7", y "15", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "21", y "27", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "11", y "22", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "24", y "17", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "31", y "26", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "26", y "22", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "30", y "17", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "29", y "9", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "24", y "32", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "14", y "30", width "1", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "14", y "26", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "14", y "16", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "6", y "25", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "10", y "30", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "26", y "33", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "17", y "32", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "11", y "33", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "24", y "26", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "28", y "22", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "32", y "18", width "5", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "26", y "17", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "26", y "29", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "21", y "2", width "5", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "29", y "6", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "22", y "5", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "8", y "6", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "18", y "6", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "28", y "3", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "11", y "1", width "5", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "13", y "4", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "21", y "13", width "6", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "10", y "13", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "14", y "12", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "4", y "20", width "5", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "13", y "22", width "5", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "22", y "20", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "16", y "19", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "30", y "13", width "4", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "23", y "8", width "4", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "32", y "9", width "2", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "3", y "15", width "3", height "1" ] []
        , rect [ fill Colors.deepOceanObjectColorAsHex, x "9", y "16", width "3", height "1" ] []
        ]
