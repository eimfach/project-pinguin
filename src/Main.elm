module Main exposing (..)

import Assets.Forest
import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onMouseOver)
import List.Extra
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
    , landmassGeneration :
        { coordinate : Maybe Coordinate
        , possibleCoordinates : Maybe (List Coordinate)
        , pickedBiome : Maybe World.Biome
        , biomes : Maybe (List World.Biome)
        , biomeIndex : Maybe Int
        , worldMapGrid : Maybe (List World.Chunk)
        , ecoSystemGrid : Maybe (List World.Chunk)
        }
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
        { coordinate = Nothing
        , possibleCoordinates = Nothing
        , pickedBiome = Nothing
        , biomes = Nothing
        , biomeIndex = Nothing
        , worldMapGrid = Nothing
        , ecoSystemGrid = Nothing
        }
        Nothing
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Roll
    | NewDiceFacesForBiomeGeneration (List.Nonempty.Nonempty World.Biome) World.EcoSystemType (List Int)
    | StartLandMassGeneration
    | LandmassGenerationStepper World.GenerationStep
    | PickRandomCoordinate (List Coordinate) World.GenerationStep Int
    | PickRandomBiome (List World.Biome) World.GenerationStep Int
    | DisplayChunkInfo World.Chunk



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
    [ Random.generate (NewDiceFacesForBiomeGeneration regularSeedList ecoSystemType) regularGenerator
    , Random.generate (NewDiceFacesForBiomeGeneration seldomSeedList ecoSystemType) seldomGenerator
    , Random.generate (NewDiceFacesForBiomeGeneration rareSeedList ecoSystemType) rareGenerator
    , Random.generate (NewDiceFacesForBiomeGeneration uniqueSeedList ecoSystemType) uniqueGenerator
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickRandomBiome biomes nextStep randomIndex ->
            let
                { landmassGeneration } =
                    model

                newLandmassGenerationProps =
                    { landmassGeneration
                        | pickedBiome = List.Extra.getAt randomIndex biomes
                        , biomes = Just biomes
                        , biomeIndex = Just randomIndex
                    }

                newModel =
                    { model | landmassGeneration = newLandmassGenerationProps }
            in
            update (LandmassGenerationStepper nextStep) newModel

        PickRandomCoordinate coordinates nextStep randomIndex ->
            let
                { landmassGeneration } =
                    model

                updatedLandmassGeneration =
                    { landmassGeneration | coordinate = List.Extra.getAt randomIndex coordinates }

                newModel =
                    { model | landmassGeneration = updatedLandmassGeneration }
            in
            update (LandmassGenerationStepper nextStep) newModel

        LandmassGenerationStepper step ->
            case step of
                World.PickRandomCoordinate createGenerator coordinates nextStep ->
                    case model.landmassGeneration.possibleCoordinates of
                        Just currentPossibleCoordinates ->
                            ( model, Random.generate (PickRandomCoordinate coordinates nextStep) (createGenerator currentPossibleCoordinates) )

                        Nothing ->
                            ( model, Random.generate (PickRandomCoordinate coordinates nextStep) (createGenerator []) )

                World.PickRandomBiome createGenerator biomes nextStep ->
                    case model.landmassGeneration.biomes of
                        Just updatedBiomes ->
                            ( model, Random.generate (PickRandomBiome biomes nextStep) <| createGenerator updatedBiomes )

                        Nothing ->
                            ( model, Random.generate (PickRandomBiome biomes nextStep) <| createGenerator biomes )

                World.ReplaceChunkBiomeByCoordinate worldMapGrid replaceBiome nextStep ->
                    case ( model.landmassGeneration.coordinate, model.landmassGeneration.pickedBiome ) of
                        ( Just aCoordinate, Just aBiome ) ->
                            let
                                { landmassGeneration } =
                                    model

                                { ecoSystemGrid } =
                                    landmassGeneration

                                ( maybeChunk, updatedWorldMapGrid ) =
                                    replaceBiome aBiome aCoordinate worldMapGrid
                            in
                            case maybeChunk of
                                Just theChunk ->
                                    let
                                        updatedEcoSystemGrid =
                                            case ecoSystemGrid of
                                                Just currentGrid ->
                                                    Just <| theChunk :: currentGrid

                                                Nothing ->
                                                    Just [ theChunk ]

                                        updatedLandMassGeneration =
                                            { landmassGeneration
                                                | worldMapGrid = Just updatedWorldMapGrid
                                                , ecoSystemGrid = updatedEcoSystemGrid
                                            }
                                    in
                                    update
                                        (LandmassGenerationStepper nextStep)
                                        { model
                                            | worldMapGrid = updatedWorldMapGrid
                                            , landmassGeneration = updatedLandMassGeneration
                                        }

                                Nothing ->
                                    ( { model | error = Just "Landmass Generation: Critical Error trying to replace a chunks biome." }, Cmd.none )

                        _ ->
                            ( { model | error = Just "Landmass Generation: Missing random biome or coordinate at model.landmassGeneration" }, Cmd.none )

                World.DropPickedBiomeFromBiomeList nextStep ->
                    let
                        { landmassGeneration } =
                            model
                    in
                    case ( landmassGeneration.biomeIndex, landmassGeneration.biomes ) of
                        ( Just theIndex, Just theBiomes ) ->
                            let
                                updatedLandmassGeneration =
                                    { landmassGeneration
                                        | biomes = Just <| List.Extra.removeAt theIndex theBiomes
                                    }

                                updatedModel =
                                    { model | landmassGeneration = updatedLandmassGeneration }
                            in
                            update (LandmassGenerationStepper nextStep) updatedModel

                        _ ->
                            ( { model | error = Just "Landmass Generation: Missing random biomeIndex" }, Cmd.none )

                World.CalculatePossibleCoordinates calculateCoordinates nextStep ->
                    case ( model.landmassGeneration.ecoSystemGrid, model.landmassGeneration.worldMapGrid ) of
                        ( Just ecoSystemGrid, Just worldMapGrid ) ->
                            let
                                { landmassGeneration } =
                                    model

                                updatedPossibleCoordinates =
                                    calculateCoordinates ecoSystemGrid worldMapGrid

                                updatedLandmassGeneration =
                                    { landmassGeneration | possibleCoordinates = Just updatedPossibleCoordinates }
                            in
                            update (LandmassGenerationStepper nextStep) { model | landmassGeneration = updatedLandmassGeneration }

                        _ ->
                            ( { model | error = Just "Landmass Generation: Error updating possible coordinates" }, Cmd.none )

                World.EndStep ->
                    -- apply model.landmassGeneration.worldMapGrid to root grid
                    ( model, Cmd.none )

        StartLandMassGeneration ->
            let
                biomes =
                    World.EcoSystemTypeDict.get World.ModerateEcoSystemType model.generatedEcoSystems
            in
            case biomes of
                Just generatedBiomes ->
                    let
                        generationSteps =
                            World.addLandmassDistribution
                                (World.Continents World.OneContinent)
                                model.worldMapGrid
                                (List.Nonempty.toList generatedBiomes)
                    in
                    update (LandmassGenerationStepper generationSteps) model

                Nothing ->
                    ( { model | error = Just "Landmass Generation: Could not find requested biomes from generation dict." }, Cmd.none )

        DisplayChunkInfo chunk ->
            ( { model | displayCoordinates = Just chunk.coordinate }, Cmd.none )

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

        NewDiceFacesForBiomeGeneration seedList ecoSystemType randomList ->
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
    let
        landMassGenerationButton =
            if List.isEmpty model.worldMapGrid then
                Html.text "Press roll to generate the grid and random biomes for the ecosystems"

            else
                button [ onClick StartLandMassGeneration ] [ Html.text "StartLandMassGeneration" ]
    in
    div []
        [ button [ onClick Roll ]
            [ Html.text "Roll" ]
        , landMassGenerationButton
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
                , Assets.Forest.mixedForest
                ]
            , g [ class "pod-wrap" ]
                (List.map (\chunk -> generateHex chunk) worldMapGrid)
            ]

        -- (List.map (\chunk -> generateHex chunk) worldMapGrid)
        ]


generateHex : World.Chunk -> Html Msg
generateHex chunk =
    use
        [ Svg.Attributes.xlinkHref <| "#" ++ chooseSvgAssetId chunk.biome
        , Svg.Attributes.transform <| createTranslateValue chunk.coordinate.x chunk.coordinate.y
        , class <| chooseColors chunk.biome
        , onMouseOver (DisplayChunkInfo chunk)
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


chooseSvgAssetId : World.Biome -> String
chooseSvgAssetId biome =
    case biome of
        World.Forest _ _ _ _ ->
            "mixed-forest"

        _ ->
            "pod"


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
            "generic-ocean"

        World.Rock _ _ _ _ ->
            "generic-landmass"

        World.Forest (World.MixedForest _) _ _ _ ->
            "generic-landmass"

        _ ->
            "generic-landmass"
