module Main exposing (..)

import Assets.Basic
import Assets.Forest
import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onMouseOver)
import Html.Lazy
import List.Extra
import List.Nonempty
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy
import Time
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
        , subscriptions = subscriptions
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
        , createdChunk : Maybe World.Chunk
        , biomes : Maybe (List World.Biome)
        , biomeIndex : Maybe Int
        , ecoSystemGrid : Maybe (List World.Chunk)
        }
    , generationSteps : Maybe (List World.GenerationStep)
    , error : Maybe String
    , timeSinceLastFrame : Int
    , fps : Int
    , highestFPS : Int
    , lowestFPS : Int
    , startTime : Maybe Int
    , endTime : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            update Roll
                (Model
                    World.SmallEcoSystem
                    [ World.ModerateEcoSystemType, World.MoonEcoSystemType ]
                    World.EcoSystemTypeDict.empty
                    []
                    Nothing
                    emptyLandmassGeneration
                    Nothing
                    Nothing
                    0
                    0
                    0
                    0
                    Nothing
                    Nothing
                )
    in
    ( model, cmds )


emptyLandmassGeneration =
    { coordinate = Nothing
    , possibleCoordinates = Nothing
    , pickedBiome = Nothing
    , createdChunk = Nothing
    , biomes = Nothing
    , biomeIndex = Nothing
    , ecoSystemGrid = Nothing
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.generationSteps of
        Just _ ->
            Time.every 0 LandmassGenerationStepper

        Nothing ->
            Sub.none



---- UPDATE ----


type Msg
    = Roll
    | NewDiceFacesForBiomeGeneration (List.Nonempty.Nonempty World.Biome) World.EcoSystemType (List Int)
    | NewDiceFacesChunkTreesCoordinates World.Chunk (List Coordinate)
    | NewDiceFacesChunkTreeTypes World.Chunk (List World.TreeType)
    | StartLandMassGeneration
    | LandmassGenerationStepper Time.Posix
    | NewFaceRandomCoordinate (List Coordinate) Int
    | NewFaceRandomBiome (List World.Biome) Int
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


updateGenerationSteps : Model -> Maybe (List World.GenerationStep) -> Model
updateGenerationSteps model steps =
    case steps of
        Just theRemainingSteps ->
            if List.length theRemainingSteps == 0 then
                { model | generationSteps = Nothing }

            else
                { model | generationSteps = steps }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartLandMassGeneration ->
            let
                biomes =
                    World.EcoSystemTypeDict.get World.ModerateEcoSystemType model.generatedEcoSystems
            in
            case biomes of
                Just generatedBiomes ->
                    let
                        { landmassGeneration } =
                            model

                        updatedLandMassGeneration =
                            { landmassGeneration | biomes = Just <| List.Nonempty.toList generatedBiomes }

                        generationSteps =
                            World.addLandmassDistribution
                                (World.Continents World.OneContinent)
                                (World.createWorldMapGrid model.ecoSystemsSize)
                                (List.Nonempty.toList generatedBiomes)
                    in
                    ( { model
                        | generationSteps = Just generationSteps
                        , landmassGeneration = updatedLandMassGeneration
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | error = Just "Landmass Generation: Could not find requested biomes from generation dict." }, Cmd.none )

        NewFaceRandomBiome biomes randomIndex ->
            let
                { landmassGeneration } =
                    model

                newLandmassGenerationProps =
                    { landmassGeneration
                        | pickedBiome = List.Extra.getAt randomIndex biomes
                        , biomeIndex = Just randomIndex
                    }

                updatedModel =
                    { model
                        | landmassGeneration = newLandmassGenerationProps
                    }
            in
            ( updatedModel, Cmd.none )

        NewFaceRandomCoordinate coordinates randomIndex ->
            let
                { landmassGeneration } =
                    model

                updatedLandmassGeneration =
                    { landmassGeneration | coordinate = List.Extra.getAt randomIndex coordinates }

                updatedModel =
                    { model
                        | landmassGeneration = updatedLandmassGeneration
                    }
            in
            ( updatedModel, Cmd.none )

        NewDiceFacesChunkTreesCoordinates theChunk coordinates ->
            let
                updatedChunk =
                    World.mapCoordinatesToChunkTrees theChunk coordinates

                { landmassGeneration } =
                    model

                updatedLandmassGeneration =
                    { landmassGeneration | createdChunk = Just updatedChunk }
            in
            ( { model | landmassGeneration = updatedLandmassGeneration }, Cmd.none )

        NewDiceFacesChunkTreeTypes theChunk treeTypes ->
            let
                updatedChunk =
                    World.mapTreeTypesToChunkTrees theChunk treeTypes

                { landmassGeneration } =
                    model

                updatedLandmassGeneration =
                    { landmassGeneration | createdChunk = Just updatedChunk }
            in
            ( { model | landmassGeneration = updatedLandmassGeneration }, Cmd.none )

        LandmassGenerationStepper time ->
            let
                fps =
                    1000 // (Time.posixToMillis time - model.timeSinceLastFrame)

                startTime =
                    case model.startTime of
                        Just aTime ->
                            Just aTime

                        Nothing ->
                            Just <| Time.posixToMillis time

                modelWithFrameTime =
                    { model
                        | fps = fps
                        , timeSinceLastFrame = Time.posixToMillis time
                        , startTime = startTime
                    }

                ( currentStep, nextSteps ) =
                    case model.generationSteps of
                        Just theStepList ->
                            ( List.head theStepList, List.tail theStepList )

                        Nothing ->
                            ( Nothing, Nothing )
            in
            case currentStep of
                Just (World.RollRandomCoordinate createGenerator coordinates) ->
                    case model.landmassGeneration.possibleCoordinates of
                        Just currentPossibleCoordinates ->
                            ( updateGenerationSteps modelWithFrameTime nextSteps, Random.generate (NewFaceRandomCoordinate currentPossibleCoordinates) (createGenerator currentPossibleCoordinates) )

                        Nothing ->
                            ( updateGenerationSteps modelWithFrameTime nextSteps, Random.generate (NewFaceRandomCoordinate coordinates) (createGenerator []) )

                Just (World.RollRandomBiome createGenerator) ->
                    case model.landmassGeneration.biomes of
                        Just updatedBiomes ->
                            ( updateGenerationSteps modelWithFrameTime nextSteps, Random.generate (NewFaceRandomBiome updatedBiomes) <| createGenerator updatedBiomes )

                        Nothing ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [World.RollRandomBiome]: Missing generated biome list" } Nothing, Cmd.none )

                Just (World.CreateChunk createChunk) ->
                    case ( model.landmassGeneration.coordinate, model.landmassGeneration.pickedBiome ) of
                        ( Just aCoordinate, Just aBiome ) ->
                            let
                                { landmassGeneration } =
                                    model

                                newChunk =
                                    createChunk aBiome aCoordinate

                                updatedLandMassGeneration =
                                    { landmassGeneration
                                        | createdChunk = Just newChunk
                                    }
                            in
                            ( updateGenerationSteps
                                { modelWithFrameTime
                                    | landmassGeneration = updatedLandMassGeneration
                                }
                                nextSteps
                            , Cmd.none
                            )

                        ( Nothing, Just _ ) ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Missing random biome at model.landmassGeneration" } Nothing, Cmd.none )

                        ( Just _, Nothing ) ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Missing random coordinate at model.landmassGeneration" } Nothing, Cmd.none )

                        ( Nothing, Nothing ) ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Missing random biome and random coordinate at model.landmassGeneration" } Nothing, Cmd.none )

                Just (World.RollChunkTreesSubCoordinates createGenerator) ->
                    case model.landmassGeneration.createdChunk of
                        Just aNewChunk ->
                            case createGenerator aNewChunk of
                                Just generator ->
                                    ( updateGenerationSteps modelWithFrameTime nextSteps, Random.generate (NewDiceFacesChunkTreesCoordinates aNewChunk) generator )

                                Nothing ->
                                    ( updateGenerationSteps modelWithFrameTime nextSteps, Cmd.none )

                        Nothing ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [RollChunkTreesSubCoordinates]: Missing created Chunk" } Nothing, Cmd.none )

                Just (World.RollChunkTreeTypes createGenerator) ->
                    case model.landmassGeneration.createdChunk of
                        Just aNewChunk ->
                            case createGenerator aNewChunk of
                                Just generator ->
                                    ( updateGenerationSteps modelWithFrameTime nextSteps, Random.generate (NewDiceFacesChunkTreeTypes aNewChunk) generator )

                                Nothing ->
                                    ( updateGenerationSteps modelWithFrameTime nextSteps, Cmd.none )

                        Nothing ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [RollChunkTreeTypes]: Missing created Chunk" } Nothing, Cmd.none )

                Just World.AddChunkToList ->
                    let
                        { landmassGeneration } =
                            model

                        { ecoSystemGrid } =
                            landmassGeneration
                    in
                    case landmassGeneration.createdChunk of
                        Just theNewChunk ->
                            let
                                updatedEcoSystemGrid =
                                    case ecoSystemGrid of
                                        Just currentGrid ->
                                            Just <| theNewChunk :: currentGrid

                                        Nothing ->
                                            Just [ theNewChunk ]

                                updatedLandMassGeneration =
                                    { landmassGeneration
                                        | ecoSystemGrid = updatedEcoSystemGrid
                                    }
                            in
                            ( updateGenerationSteps { modelWithFrameTime | landmassGeneration = updatedLandMassGeneration } nextSteps, Cmd.none )

                        Nothing ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Error merging generated Chunk" } Nothing, Cmd.none )

                Just World.DropPickedBiomeFromBiomeList ->
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
                                    { modelWithFrameTime
                                        | landmassGeneration = updatedLandmassGeneration
                                    }
                            in
                            ( updateGenerationSteps updatedModel nextSteps, Cmd.none )

                        _ ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Missing random biomeIndex" } Nothing, Cmd.none )

                Just (World.CalculatePossibleCoordinates calculateCoordinates) ->
                    case model.landmassGeneration.ecoSystemGrid of
                        Just ecoSystemGrid ->
                            let
                                { landmassGeneration } =
                                    model

                                newPossibleCoordinates =
                                    calculateCoordinates ecoSystemGrid

                                updatedLandmassGeneration =
                                    { landmassGeneration | possibleCoordinates = Just newPossibleCoordinates }
                            in
                            ( updateGenerationSteps { modelWithFrameTime | landmassGeneration = updatedLandmassGeneration } nextSteps, Cmd.none )

                        _ ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Error refreshing possible coordinates." } Nothing, Cmd.none )

                Just World.EndStep ->
                    let
                        endTime =
                            Just <| Time.posixToMillis time
                    in
                    case model.landmassGeneration.ecoSystemGrid of
                        Just ecoSystemGrid ->
                            -- insert ecoSystemGrid into worldMapGrid
                            let
                                worldMapGrid =
                                    World.createWorldMapGrid model.ecoSystemsSize

                                updatedWorldMap =
                                    -- quite unsafe, regarding duplication or replacing duplicate coordinates ?
                                    List.foldl
                                        (\newChunk worldMap ->
                                            List.Extra.setIf
                                                (\worldChunk -> worldChunk.coordinate == newChunk.coordinate)
                                                newChunk
                                                worldMap
                                        )
                                        worldMapGrid
                                        ecoSystemGrid
                            in
                            ( updateGenerationSteps { modelWithFrameTime | worldMapGrid = updatedWorldMap, endTime = endTime } nextSteps, Cmd.none )

                        _ ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Could not update WorldMapGrid", endTime = endTime } Nothing, Cmd.none )

                Nothing ->
                    ( modelWithFrameTime, Cmd.none )

        DisplayChunkInfo chunk ->
            ( { model | displayCoordinates = Just chunk.coordinate }, Cmd.none )

        Roll ->
            let
                commands : List (Cmd Msg)
                commands =
                    List.map (rollDicesForEcoSystemType model.ecoSystemsSize) model.ecoSystemTypes
                        |> List.foldl List.append []
            in
            ( model
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
                    ( { model | error = Just "[NewDiceFacesForBiomeGeneration] Error while adding generated biomes" }
                    , Cmd.none
                    )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        worldMap =
            Html.Lazy.lazy generateHexes model.worldMapGrid

        generationTime =
            case ( model.startTime, model.endTime ) of
                ( Just startTime, Just endTime ) ->
                    (String.fromFloat <| (toFloat endTime - toFloat startTime) / 1000) ++ "s"

                _ ->
                    "N/A"
    in
    div []
        [ Html.text <| Maybe.withDefault "" model.error
        , button [ onClick StartLandMassGeneration ] [ Html.text "Start" ]
        , div [ Html.Attributes.id "coordinates-display" ]
            [ div [] [ Html.text <| convertCoordinate model.displayCoordinates ]
            , div [] [ Html.text "FPS: " ]
            , div [] [ Html.text <| String.fromInt model.fps ]
            , div [] [ Html.text "Highest FPS: " ]
            , div [] [ Html.text <| String.fromInt model.highestFPS ]
            , div [] [ Html.text "Lowest FPS: " ]
            , div [] [ Html.text <| String.fromInt model.lowestFPS ]
            , div [] [ Html.text <| "Generation Time Spent: " ++ generationTime ]
            ]
        , worldMap
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
                (List.append (generateForestHexParents worldMapGrid) [ Assets.Basic.generic ])
            , g [ class "pod-wrap" ]
                (List.map (\chunk -> Html.Lazy.lazy generateHex chunk) worldMapGrid)
            ]
        ]


generateForestHexParents : List World.Chunk -> List (Svg msg)
generateForestHexParents chunks =
    List.map (Svg.Lazy.lazy Assets.Forest.genericForest) (World.filterForestChunks chunks)


generateHex : World.Chunk -> Html Msg
generateHex chunk =
    let
        assetID =
            case chunk.biome of
                World.Forest _ _ _ _ ->
                    World.coordinatesToString chunk.coordinate

                _ ->
                    "pod"
    in
    use
        [ Svg.Attributes.xlinkHref <| "#" ++ assetID
        , Svg.Attributes.transform <| createTranslateValue chunk.coordinate.x chunk.coordinate.y
        , class <| chooseColors chunk.biome
        , Html.Events.onMouseDown (DisplayChunkInfo chunk)
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
    Svg.text_ [ width "20", height "20", fill "black", class "small" ] [ Svg.text text ]


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
