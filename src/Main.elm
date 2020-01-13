module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onMouseOver)
import List.Nonempty
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import World
import World.EcoSystemTypeDict



---- TYPES ----


type alias Coordinate =
    { x : Int, y : Int }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    { ecoSystemsSize : World.EcoSystemSize
    , ecoSystemTypes : List World.EcoSystemType
    , generatedEcoSystems : World.EcoSystemTypeDict.EcoSystemTypeDict
    , worldMapGrid : List World.Chunk
    , displayCoordinates : Maybe Coordinate
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        World.SmallEcoSystem
        [ World.ModerateEcoSystemType, World.MoonEcoSystemType ]
        World.EcoSystemTypeDict.empty
        []
        Nothing
        Nothing
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Roll
    | NewFace (List.Nonempty.Nonempty World.Biome) World.EcoSystemType (List Int)
    | DisplayCoordinates Coordinate



-- | ShuffleGeneratedBiomes (List World.Biome)


{-|

  - rollDicesForEcoSystemType:
  - Will create a batch of roll commands, where for all commands applies that they are roll actions for the given
    `World.EcoSystemType`.
  - Additionally each command is a roll action for a specific seedList and has different rolling properties than the other commands.
  - The given `World.EcoSystemSize` tells how much times to roll on a List as a basic value and will give small or larger results.
  - The the generated Lists contain related indicies to choose biomes from the specific seedLists.

-}
rollDicesForEcoSystemType : World.EcoSystemSize -> World.EcoSystemType -> List (Cmd Msg)
rollDicesForEcoSystemType ecosystemSize ecoSystemType =
    let
        getEcoSystemSeedingProperties =
            World.getEcoSystemBiomeSeedingProperties ecosystemSize ecoSystemType

        ( regularSeedList, regularGenerator ) =
            getEcoSystemSeedingProperties World.RegularOccurrence
                |> World.seedingPropertiesToTuple

        ( seldomSeedList, seldomGenerator ) =
            getEcoSystemSeedingProperties World.SeldomOccurrence
                |> World.seedingPropertiesToTuple

        ( rareSeedList, rareGenerator ) =
            getEcoSystemSeedingProperties World.RareOccurrence
                |> World.seedingPropertiesToTuple

        ( uniqueSeedList, uniqueGenerator ) =
            getEcoSystemSeedingProperties World.UniqueOccurrence
                |> World.seedingPropertiesToTuple
    in
    [ Random.generate (NewFace regularSeedList ecoSystemType) regularGenerator
    , Random.generate (NewFace seldomSeedList ecoSystemType) seldomGenerator
    , Random.generate (NewFace rareSeedList ecoSystemType) rareGenerator
    , Random.generate (NewFace uniqueSeedList ecoSystemType) uniqueGenerator
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayCoordinates coordinate ->
            ( { model | displayCoordinates = Just coordinate }, Cmd.none )

        Roll ->
            let
                commands : List (Cmd Msg)
                commands =
                    List.map (rollDicesForEcoSystemType model.ecoSystemsSize) model.ecoSystemTypes
                        |> List.foldl List.append []
            in
            ( { model | worldMapGrid = World.createWorldMapGrid model.ecoSystemsSize }
            , Cmd.batch commands
            )

        NewFace seedList ecoSystemType randomList ->
            let
                newGeneratedBiomes =
                    List.map (\randomIndex -> List.Nonempty.get randomIndex seedList) randomList
                        |> List.Nonempty.fromList
            in
            case ( World.EcoSystemTypeDict.get ecoSystemType model.generatedEcoSystems, newGeneratedBiomes ) of
                ( Just biomesForUpdate, Just newBiomes ) ->
                    let
                        updatedBiomes =
                            List.Nonempty.append biomesForUpdate newBiomes
                    in
                    ( { model | generatedEcoSystems = World.EcoSystemTypeDict.insert ecoSystemType updatedBiomes model.generatedEcoSystems }
                    , Cmd.none
                    )

                ( Nothing, Just newBiomes ) ->
                    ( { model | generatedEcoSystems = World.EcoSystemTypeDict.insert ecoSystemType newBiomes model.generatedEcoSystems }
                    , Cmd.none
                    )

                _ ->
                    ( { model | error = Just "Error while adding generated biomes" }
                    , Cmd.none
                    )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ]
            [ Html.text "Roll" ]
        , div [ Html.Attributes.id "coordinates-display" ]
            [ Html.text <| convertCoordinate model.displayCoordinates ]
        , generateHexes model.worldMapGrid
        ]


convertCoordinate : Maybe Coordinate -> String
convertCoordinate coordinate =
    case coordinate of
        Just { x, y } ->
            "x: " ++ String.fromInt x ++ " || " ++ "y: " ++ String.fromInt y

        Nothing ->
            "Display Coordinate"


generateHexes : List World.Chunk -> Html Msg
generateHexes worldMapGrid =
    div []
        [ svg [ viewBox "0 0 1200 1200" ]
            [ defs []
                [ g [ id "pod" ]
                    [ polygon
                        [ stroke "#000000"
                        , strokeWidth "0.5"
                        , points "5,-9 -5,-9 -10,0 -5,9 5,9 10,0"
                        ]
                        []
                    ]
                ]
            , g [ class "pod-wrap" ]
                (List.map (\chunk -> generateHex chunk) worldMapGrid)
            ]

        -- (List.map (\chunk -> generateHex chunk) worldMapGrid)
        ]


generateHex : World.Chunk -> Html Msg
generateHex chunk =
    use
        [ Svg.Attributes.xlinkHref "#pod"
        , Svg.Attributes.transform <| createTranslateValue chunk.coordinate.x chunk.coordinate.y
        , class "ocean-saltyWaterOcean"
        , onMouseOver (DisplayCoordinates chunk.coordinate)
        ]
        []


createTranslateValue : Int -> Int -> String
createTranslateValue nativeX nativeY =
    let
        { x, y } =
            calculateTranslateCoordinates { nativeX = nativeX, nativeY = nativeY }
    in
    "translate(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


{-|

  - `calculateTranslateCoordinates`

  - Translates given x and y coordinates from the World Module to the rendered Hex Grid coordinates.

  - The calculated Hex Grid coordinates are absolute specific to the rendered SVG Properties (which uses `translate` attribute).

    calculateTranslateCoordinates {nativeX = 2, nativeY = 3} --> { x = 40, y = 64}

    calculateTranslateCoordinates {nativeX = 3, nativeY = 3} --> { x = 55, y = 73}

    calculateTranslateCoordinates {nativeX = 5, nativeY = 3} --> { x = 85, y = 73}

-}
calculateTranslateCoordinates : { nativeX : Int, nativeY : Int } -> { x : Int, y : Int }
calculateTranslateCoordinates { nativeX, nativeY } =
    let
        x =
            if nativeX == 0 then
                10

            else
                10 + (nativeX * 15)

        y =
            if modBy 2 nativeX == 1 then
                19 + (nativeY * 18)

            else
                10 + (nativeY * 18)
    in
    { x = x, y = y }


chooseBiomeText : World.Biome -> Svg msg
chooseBiomeText biome =
    case biome of
        World.Forest (World.MixedForest _) _ _ _ ->
            svgTextNode "MixedForest"

        World.Plane World.MixedPlane _ _ _ ->
            svgTextNode "MixedPlane"

        World.Forest (World.DarkForest _) _ _ _ ->
            svgTextNode "DarkForest"

        World.Plane (World.RiverPlane _) _ _ _ ->
            svgTextNode "RiverPlane"

        World.Forest World.RiverForest _ _ _ ->
            svgTextNode "RiverForest"

        World.Rock World.GreyRock _ _ _ ->
            svgTextNode "GreyRock"

        World.Rock World.DarkRock _ _ _ ->
            svgTextNode "DarkRock"

        World.Forest World.MagicForest _ _ _ ->
            svgTextNode "MagicForest"

        World.Forest World.LivingForest _ _ _ ->
            svgTextNode "LivingForest"

        World.Forest World.DeepForest _ _ _ ->
            svgTextNode "DeepForest"

        World.Lake World.WaterLake _ _ _ ->
            svgTextNode "WaterLake"

        World.Rock World.RiverRock _ _ _ ->
            svgTextNode "RiverRock"

        World.Plane World.MagicPlane _ _ _ ->
            svgTextNode "MagicPlane"

        World.Forest World.DreamForest _ _ _ ->
            svgTextNode "DreamForest"

        World.River World.WaterRiver _ _ _ ->
            svgTextNode "WaterRiver"

        _ ->
            svgTextNode "Unknown"


svgTextNode text =
    Svg.text_ [ x "100", y "100", fill "white", class "small" ] [ Svg.text text ]


chooseColors : World.Biome -> String
chooseColors biome =
    case biome of
        World.Ocean World.SaltyWaterOcean _ _ _ ->
            "blue"

        _ ->
            "green"
