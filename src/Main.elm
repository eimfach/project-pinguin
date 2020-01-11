module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List.Nonempty
import Random
import Random.List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import World
import World.EcoSystemTypeDict



---- TYPES ----
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
    { ecosystemsSize : World.EcoSystemSize
    , ecoSystemTypes : List World.EcoSystemType
    , generatedEcoSystems : World.EcoSystemTypeDict.EcoSystemTypeDict
    , worldMapGrid : List World.Chunk
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
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Roll
    | NewFace (List.Nonempty.Nonempty World.Biome) World.EcoSystemType (List Int)
      -- | ShuffleGeneratedBiomes (List World.Biome)
    | CreateGrid


rollDiceForEcoSystemType : World.EcoSystemSize -> World.EcoSystemType -> List (Cmd Msg)
rollDiceForEcoSystemType ecosystemSize ecoSystemType =
    let
        getSmallModerateEcoSystemSeedingProperties =
            World.getEcoSystemBiomeSeedingProperties ecosystemSize ecoSystemType

        regularSeedingProps =
            getSmallModerateEcoSystemSeedingProperties World.RegularOccurrence

        seldomSeedingProps =
            getSmallModerateEcoSystemSeedingProperties World.SeldomOccurrence

        rareSeedingProps =
            getSmallModerateEcoSystemSeedingProperties World.RareOccurrence

        uniqueSeedingProps =
            getSmallModerateEcoSystemSeedingProperties World.UniqueOccurrence
    in
    [ Random.generate (NewFace regularSeedingProps.seedList ecoSystemType) (generateRollProperties regularSeedingProps.share <| List.Nonempty.length regularSeedingProps.seedList)
    , Random.generate (NewFace seldomSeedingProps.seedList ecoSystemType) (generateRollProperties seldomSeedingProps.share <| List.Nonempty.length seldomSeedingProps.seedList)
    , Random.generate (NewFace rareSeedingProps.seedList ecoSystemType) (generateRollProperties rareSeedingProps.share <| List.Nonempty.length rareSeedingProps.seedList)
    , Random.generate (NewFace uniqueSeedingProps.seedList ecoSystemType) (generateRollProperties uniqueSeedingProps.share <| List.Nonempty.length uniqueSeedingProps.seedList)
    ]


generateRollProperties rolls diceFaceCount =
    Random.list rolls <| Random.int 0 diceFaceCount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                commands : List (Cmd Msg)
                commands =
                    List.map (rollDiceForEcoSystemType model.ecosystemsSize) model.ecoSystemTypes
                        |> List.foldl List.append []
            in
            ( model
            , Cmd.batch commands
            )

        NewFace seedList ecoSystemType randomList ->
            let
                newGeneratedBiomes =
                    List.map (\randomIndex -> List.Nonempty.get randomIndex seedList) randomList
                        |> List.Nonempty.fromList
            in
            case ( World.EcoSystemTypeDict.get ecoSystemType model.generatedEcoSystems, newGeneratedBiomes ) of
                ( Just currentBiomes, Just nonEmptyNewBiomes ) ->
                    let
                        updatedBiomes =
                            List.Nonempty.append currentBiomes nonEmptyNewBiomes
                    in
                    ( { model | generatedEcoSystems = World.EcoSystemTypeDict.insert ecoSystemType updatedBiomes model.generatedEcoSystems }
                    , Cmd.none
                    )

                ( Nothing, Just nonEmptyNewBiomes ) ->
                    ( { model | generatedEcoSystems = World.EcoSystemTypeDict.insert ecoSystemType nonEmptyNewBiomes model.generatedEcoSystems }
                    , Cmd.none
                    )

                _ ->
                    ( { model | error = Just "Error while adding generated biomes" }
                    , Cmd.none
                    )

        CreateGrid ->
            ( { model | worldMapGrid = World.createWorldMapGrid model.ecosystemsSize }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ Html.text "Roll" ]
        , button [ onClick CreateGrid ] [ Html.text "Create Grid" ]
        , generateHexes model.worldMapGrid
        ]


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
        ]
        []


createTranslateValue : Int -> Int -> String
createTranslateValue nativeX nativeY =
    let
        { x, y } =
            calculateTranslateCoordinates nativeX nativeY
    in
    "translate(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


calculateTranslateCoordinates : Int -> Int -> { x : Int, y : Int }
calculateTranslateCoordinates nativeX nativeY =
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
