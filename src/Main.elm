module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List.Nonempty
import Random
import Random.List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import World



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
    { generatedBiomes : List World.Biome
    }


init : ( Model, Cmd Msg )
init =
    ( Model []
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Roll
    | NewFace (List.Nonempty.Nonempty World.Biome) (List Int)
    | ShuffleGeneratedBiomes (List World.Biome)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                getSmallModerateEcoSystemSeedingProperties =
                    World.getEcoSystemBiomeSeedingProperties World.SmallEcoSystem World.ModerateEcoSystemType

                regularSeedingProps =
                    getSmallModerateEcoSystemSeedingProperties World.RegularOccurrence

                seldomSeedingProps =
                    getSmallModerateEcoSystemSeedingProperties World.SeldomOccurrence

                rareSeedingProps =
                    getSmallModerateEcoSystemSeedingProperties World.RareOccurrence

                uniqueSeedingProps =
                    getSmallModerateEcoSystemSeedingProperties World.UniqueOccurrence
            in
            ( { model | generatedBiomes = [] }
            , Cmd.batch
                [ Random.generate (NewFace regularSeedingProps.seedList) (generateRollCommand regularSeedingProps.share <| List.Nonempty.length regularSeedingProps.seedList)
                , Random.generate (NewFace seldomSeedingProps.seedList) (generateRollCommand seldomSeedingProps.share <| List.Nonempty.length seldomSeedingProps.seedList)
                , Random.generate (NewFace rareSeedingProps.seedList) (generateRollCommand rareSeedingProps.share <| List.Nonempty.length rareSeedingProps.seedList)
                , Random.generate (NewFace uniqueSeedingProps.seedList) (generateRollCommand uniqueSeedingProps.share <| List.Nonempty.length uniqueSeedingProps.seedList)
                ]
            )

        NewFace seedList randomList ->
            let
                newGeneratedBiomes =
                    List.append model.generatedBiomes <| List.map (\index -> List.Nonempty.get index seedList) randomList
            in
            ( { model | generatedBiomes = newGeneratedBiomes }
            , Random.generate ShuffleGeneratedBiomes (Random.List.shuffle newGeneratedBiomes)
            )

        ShuffleGeneratedBiomes shuffledBiomeList ->
            ( { model | generatedBiomes = shuffledBiomeList }, Cmd.none )


generateRollCommand rolls diceFaceCount =
    Random.list rolls <| Random.int 0 diceFaceCount



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ Html.text "Roll" ]
        , generateHexes model.generatedBiomes
        ]


generateHexes : List World.Biome -> Html msg
generateHexes biomes =
    div [] (List.indexedMap (\index biome -> generateHex (index * 100) 0 biome) biomes)


generateHex pointX pointY biome =
    svg
        [ width "300"
        , height "300"
        , viewBox "0 0 300 300"
        ]
        [ polygon
            [ stroke "#000000"
            , strokeWidth "1.5px"
            , points "300,150 225,280 75,280 0,150 75,20 225,20"
            , fill <| chooseColors biome
            ]
            []
        , chooseBiomeText biome
        ]


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
        World.Forest (World.MixedForest _) _ _ _ ->
            "#7d977d"

        World.Plane World.MixedPlane _ _ _ ->
            "#616a41"

        World.Forest (World.DarkForest _) _ _ _ ->
            "#383d1f"

        World.Plane (World.RiverPlane _) _ _ _ ->
            "#b3e1ee"

        World.Forest World.RiverForest _ _ _ ->
            "#a1e1aa"

        World.Rock World.GreyRock _ _ _ ->
            "gray"

        World.Rock World.DarkRock _ _ _ ->
            "darkgray"

        World.Forest World.MagicForest _ _ _ ->
            "deeppink"

        World.Forest World.LivingForest _ _ _ ->
            "mediumslateblue"

        World.Forest World.DeepForest _ _ _ ->
            "submarine"

        World.Lake World.WaterLake _ _ _ ->
            "midnightblue"

        World.Plane World.MagicPlane _ _ _ ->
            "hotpink"

        World.Forest World.DreamForest _ _ _ ->
            "mediumvioletred"

        _ ->
            "black"
