module Main exposing (..)

import Assets
import Browser
import Browser.Events
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Html.Lazy
import List.Extra
import List.Nonempty
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Lazy
import Time
import World



---- TYPES ----


type alias GeneratedEcoSystem =
    ( World.EcoSystemType, List.Nonempty.Nonempty World.Biome )



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
    , generatedEcoSystems : List ( World.EcoSystemType, List.Nonempty.Nonempty World.Biome )
    , worldMapGrid : List World.Chunk
    , displayCoordinates : Maybe World.WorldSpace
    , displayBiome : Maybe World.Biome
    , landmassGeneration : LandMassGeneration
    , generationSteps : Maybe (List World.GenerationStepMsg)
    , error : Maybe String
    , timeSinceLastFrame : Int
    , fps : Int
    , highestFPS : Int
    , lowestFPS : Int
    , startTime : Maybe Int
    , endTime : Maybe Int
    }


type alias LandMassGeneration =
    { coordinate : Maybe World.WorldSpace
    , possibleCoordinates : Maybe (List World.WorldSpace)
    , pickedBiome : Maybe World.Biome
    , createdChunk : Maybe World.Chunk
    , biomes : Maybe (List World.Biome)
    , biomeIndex : Maybe Int
    , ecoSystemGrid : Maybe (List World.Chunk)
    , currentEcoSystemType : Maybe World.EcoSystemType
    }


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmds ) =
            update RollBiomesForEcosystems
                (Model
                    World.SmallEcoSystem
                    ecoSystemsToBeGenerated
                    []
                    []
                    Nothing
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


ecoSystemsToBeGenerated =
    [ World.ModerateEcoSystemType, World.ModerateEcoSystemType, World.ModerateEcoSystemType ]


emptyLandmassGeneration : LandMassGeneration
emptyLandmassGeneration =
    { coordinate = Nothing
    , possibleCoordinates = Nothing
    , pickedBiome = Nothing
    , createdChunk = Nothing
    , biomes = Nothing
    , biomeIndex = Nothing
    , ecoSystemGrid = Nothing
    , currentEcoSystemType = Nothing
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.generationSteps of
        Just _ ->
            Time.every 0 SubscriptionUpdatedTime

        Nothing ->
            Sub.none



---- UPDATE ----


{-| SomeoneDidSomethingSomewhereAndSomeHow
-}
type Msg
    = RollBiomesForEcosystems
    | NewDiceFacesForBiomeGeneration (List.Nonempty.Nonempty World.Biome) World.EcoSystemType Int (List Int)
    | NewDiceFacesChunkTreesCoordinates World.Chunk (List World.ScreenSpace)
    | NewDiceFacesChunkTreeTypes World.Chunk (List World.TreeType)
    | StartLandMassGeneration
    | SubscriptionUpdatedTime Time.Posix
    | NewFaceRandomCoordinate (List World.WorldSpace) Int
    | NewFaceRandomBiome (List World.Biome) Int
    | UserClickedHex World.Chunk Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartLandMassGeneration ->
            let
                worldMapGrid =
                    World.createWorldMapGrid model.ecoSystemsSize

                generationSteps =
                    World.createLandmassGenerationSteps worldMapGrid model.generatedEcoSystems (World.Continents World.OneContinent)
            in
            ( { model
                | generationSteps = Just generationSteps
              }
            , Cmd.none
            )

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

        NewFaceRandomCoordinate worldSpaceList randomIndex ->
            let
                { landmassGeneration } =
                    model

                updatedLandmassGeneration =
                    { landmassGeneration | coordinate = List.Extra.getAt randomIndex worldSpaceList }

                updatedModel =
                    { model
                        | landmassGeneration = updatedLandmassGeneration
                    }
            in
            ( updatedModel, Cmd.none )

        NewDiceFacesChunkTreesCoordinates theChunk screenSpaceList ->
            let
                coordinates =
                    List.map World.unwrapScreenSpace screenSpaceList

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

        SubscriptionUpdatedTime time ->
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
                            let
                                head =
                                    List.head theStepList

                                tail =
                                    List.tail theStepList
                                        |> Maybe.withDefault []
                            in
                            ( head, tail )

                        Nothing ->
                            ( Nothing, [] )
            in
            case currentStep of
                -- SomeoneDidSomethingSomewhereAndSomeHow
                Just World.DroppedGenerationDataForPerformance ->
                    ( updateGenerationSteps
                        { modelWithFrameTime
                            | landmassGeneration = emptyLandmassGeneration
                            , worldMapGrid = []
                        }
                        nextSteps
                    , Cmd.none
                    )

                Just (World.SetCurrentEcoSystemType ecoSystemType) ->
                    let
                        { landmassGeneration } =
                            model

                        updatedLandmassGeneration =
                            { landmassGeneration | currentEcoSystemType = Just ecoSystemType }
                    in
                    ( updateGenerationSteps { modelWithFrameTime | landmassGeneration = updatedLandmassGeneration } nextSteps, Cmd.none )

                Just (World.SetBiomeList biomes) ->
                    let
                        { landmassGeneration } =
                            model

                        updatedLandmassGeneration =
                            { landmassGeneration | biomes = Just biomes }
                    in
                    ( updateGenerationSteps { modelWithFrameTime | landmassGeneration = updatedLandmassGeneration } nextSteps, Cmd.none )

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
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [World.RollRandomBiome]: Missing generated biome list" } [], Cmd.none )

                Just (World.CreateChunk createChunk) ->
                    let
                        { landmassGeneration } =
                            model

                        { coordinate } =
                            landmassGeneration

                        { pickedBiome } =
                            landmassGeneration

                        { currentEcoSystemType } =
                            landmassGeneration
                    in
                    case ( coordinate, pickedBiome, currentEcoSystemType ) of
                        ( Just aCoordinate, Just aBiome, Just aEcoSystemType ) ->
                            let
                                newChunk =
                                    createChunk aEcoSystemType aBiome aCoordinate

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

                        _ ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [World.CreateChunk]: Missing data at model.landmassGeneration" } [], Cmd.none )

                Just (World.RollChunkTreesSubCoordinates createGenerator) ->
                    case model.landmassGeneration.createdChunk of
                        Just aNewChunk ->
                            let
                                generate =
                                    World.getTreeObjects aNewChunk
                                        |> List.Nonempty.fromList
                                        |> Maybe.andThen (\trees -> Just <| createGenerator trees)
                                        |> Maybe.withDefault (Random.constant [])
                                        |> Random.generate (NewDiceFacesChunkTreesCoordinates aNewChunk)
                            in
                            ( updateGenerationSteps modelWithFrameTime nextSteps, generate )

                        Nothing ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [RollChunkTreesSubCoordinates]: Missing created Chunk" } [], Cmd.none )

                Just (World.RollChunkTreeTypes createGenerator) ->
                    case model.landmassGeneration.createdChunk of
                        Just aNewChunk ->
                            let
                                generate =
                                    World.getTreeObjects aNewChunk
                                        |> List.Nonempty.fromList
                                        |> Maybe.andThen (\trees -> Just <| createGenerator trees)
                                        |> Maybe.withDefault (Random.constant [])
                                        |> Random.generate (NewDiceFacesChunkTreeTypes aNewChunk)
                            in
                            ( updateGenerationSteps modelWithFrameTime nextSteps, generate )

                        Nothing ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation [RollChunkTreeTypes]: Missing created Chunk" } [], Cmd.none )

                Just World.AddChunkToGlobalList ->
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
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Error merging generated Chunk" } [], Cmd.none )

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
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Missing random biomeIndex" } [], Cmd.none )

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
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Error refreshing possible coordinates." } [], Cmd.none )

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
                                                (\worldChunk -> worldChunk.location == newChunk.location)
                                                newChunk
                                                worldMap
                                        )
                                        worldMapGrid
                                        ecoSystemGrid
                            in
                            -- TODO: Remove worldmap update for performance testing without rendering overhead
                            ( updateGenerationSteps { modelWithFrameTime | endTime = endTime, worldMapGrid = updatedWorldMap } nextSteps, Cmd.none )

                        _ ->
                            ( updateGenerationSteps { modelWithFrameTime | error = Just "Landmass Generation: Could not update WorldMapGrid", endTime = endTime } [], Cmd.none )

                Nothing ->
                    ( modelWithFrameTime, Cmd.none )

        UserClickedHex chunk index ->
            let
                chunkWithVillage =
                    World.setInitialChunkVillage chunk

                updatedWorldMapGrid =
                    List.Extra.setAt index chunkWithVillage model.worldMapGrid
            in
            ( { model
                | displayCoordinates = Just chunk.location
                , displayBiome = Just chunk.biome
                , worldMapGrid = updatedWorldMapGrid
              }
            , Cmd.none
            )

        RollBiomesForEcosystems ->
            let
                commands : List (Cmd Msg)
                commands =
                    List.indexedMap (rollDicesForEcoSystemType model.ecoSystemsSize) model.ecoSystemTypes
                        |> List.foldl List.append []
            in
            ( model
            , Cmd.batch commands
            )

        NewDiceFacesForBiomeGeneration seedList ecoSystemType index randomList ->
            let
                newGeneratedBiomes =
                    List.map (\randomIndex -> List.Nonempty.get randomIndex seedList) randomList
                        |> List.Nonempty.fromList
            in
            case newGeneratedBiomes of
                Just theNewUnemptyBiomes ->
                    let
                        updatedGeneratedEcoSystems =
                            List.Extra.getAt index model.generatedEcoSystems
                                |> Maybe.andThen (updateGeneratedEcoSystem index model.generatedEcoSystems theNewUnemptyBiomes)
                                |> Maybe.withDefault (List.append model.generatedEcoSystems [ ( ecoSystemType, theNewUnemptyBiomes ) ])
                    in
                    ( { model | generatedEcoSystems = updatedGeneratedEcoSystems }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | error = Just "[NewDiceFacesForBiomeGeneration] Biome list is empty" }
                    , Cmd.none
                    )



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
----------------------------------------  UPDATE HELPERS  ----------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


updateGeneratedEcoSystem : Int -> List GeneratedEcoSystem -> List.Nonempty.Nonempty World.Biome -> GeneratedEcoSystem -> Maybe (List GeneratedEcoSystem)
updateGeneratedEcoSystem index generatedEcoSystems theNewUnemptyBiomes ( ecoSystemType, biomesToBeUpdated ) =
    Just <|
        List.Extra.setAt
            index
            ( ecoSystemType, List.Nonempty.append biomesToBeUpdated theNewUnemptyBiomes )
            generatedEcoSystems


{-|

  - rollDicesForEcoSystemType:
  - Will create a batch of roll commands, where for all commands applies that they are roll actions for the given
    `World.EcoSystemType`.
  - Additionally each command is a roll action for a specific seedList and has different rolling properties than the other commands.
  - The given `World.EcoSystemSize` tells how much times to roll on a List as a basic value and will give small or larger results.
  - The the generated Lists contain related indicies to choose biomes from the specific seedLists.

-}
rollDicesForEcoSystemType : World.EcoSystemSize -> Int -> World.EcoSystemType -> List (Cmd Msg)
rollDicesForEcoSystemType ecosystemSize index ecoSystemType =
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
    [ Random.generate (NewDiceFacesForBiomeGeneration regularSeedList ecoSystemType index) regularGenerator
    , Random.generate (NewDiceFacesForBiomeGeneration seldomSeedList ecoSystemType index) seldomGenerator
    , Random.generate (NewDiceFacesForBiomeGeneration rareSeedList ecoSystemType index) rareGenerator
    , Random.generate (NewDiceFacesForBiomeGeneration uniqueSeedList ecoSystemType index) uniqueGenerator
    ]


updateGenerationSteps : Model -> List World.GenerationStepMsg -> Model
updateGenerationSteps model steps =
    case List.head steps of
        Just aStep ->
            { model | generationSteps = Just steps }

        Nothing ->
            { model | generationSteps = Nothing }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        worldMap =
            Html.Lazy.lazy viewHexMap model.worldMapGrid

        generationTime =
            case ( model.startTime, model.endTime ) of
                ( Just startTime, Just endTime ) ->
                    (String.fromFloat <| (toFloat endTime - toFloat startTime) / 1000) ++ "s"

                _ ->
                    "N/A"

        progress =
            case model.generationSteps of
                Just steps ->
                    String.fromInt <| List.length steps

                Nothing ->
                    "Nothing ongoing..."
    in
    div []
        [ Html.text <| Maybe.withDefault "" model.error
        , button [ onClick StartLandMassGeneration ] [ Html.text "Start" ]
        , div [ Html.Attributes.id "coordinates-display" ]
            [ div [] [ Html.text <| convertCoordinate model.displayCoordinates ]
            , div [] [ Html.text "Selected Chunk Biome:" ]
            , div [] [ Html.text <| chooseBiomeText model.displayBiome ]
            , div [] [ Html.text "FPS: " ]
            , div [] [ Html.text <| String.fromInt model.fps ]
            , div [] [ Html.text "Highest FPS: " ]
            , div [] [ Html.text <| String.fromInt model.highestFPS ]
            , div [] [ Html.text "Lowest FPS: " ]
            , div [] [ Html.text <| String.fromInt model.lowestFPS ]
            , div [] [ Html.text <| "Generation Time Spent: " ++ generationTime ]
            , div [] [ Html.text <| "Progress: " ++ progress ]
            , div [] []
            ]
        , worldMap
        ]


convertCoordinate : Maybe World.WorldSpace -> String
convertCoordinate worldSpace =
    case worldSpace of
        Just (World.WorldSpace { x, y }) ->
            "x: " ++ String.fromInt x ++ " || " ++ "y: " ++ String.fromInt y

        Nothing ->
            "Display Coordinate"


viewHexMap : List World.Chunk -> Html Msg
viewHexMap worldMapGrid =
    div []
        [ svg [ viewBox "0 0 2400 2400" ]
            [ defs []
                (List.append (viewHexForestParents worldMapGrid)
                    [ Assets.pod { gridColor = Nothing }
                    , Assets.deepOcean { gridColor = Nothing }
                    , Assets.mixedPlane { gridColor = Nothing }
                    , Assets.genericLake { gridColor = Nothing }
                    , Assets.genericLandmass { gridColor = Nothing }
                    , Assets.village { gridColor = Nothing }
                    , Assets.pod { gridColor = Nothing }
                    ]
                )
            , g [ class "pod-wrap" ]
                (List.indexedMap (\i chunk -> Html.Lazy.lazy2 viewHex i chunk) worldMapGrid)
            ]
        ]


viewHexForestParents : List World.Chunk -> List (Svg msg)
viewHexForestParents chunks =
    List.map (Svg.Lazy.lazy (Assets.genericForest { gridColor = Nothing })) (World.filterForestChunks chunks)


viewHex : Int -> World.Chunk -> Html Msg
viewHex index chunk =
    let
        assetID =
            case chunk.biome of
                World.Forest _ _ _ _ ->
                    World.coordinatesToString <| World.unwrapWorldSpace chunk.location

                World.Ocean _ _ _ _ ->
                    "deep-ocean"

                World.Plane World.MixedPlane _ _ _ ->
                    case chunk.village of
                        World.SmallVillage ->
                            "village"

                        _ ->
                            "mixed-plane"

                World.Lake _ _ _ _ ->
                    "generic-lake"

                _ ->
                    "generic-landmass"
    in
    use
        [ Svg.Attributes.xlinkHref <| "#" ++ assetID
        , Svg.Attributes.transform <| createTranslateValue <| World.ScreenSpace (World.unwrapWorldSpace chunk.location)
        , Html.Events.onClick (UserClickedHex chunk index)
        ]
        []


createTranslateValue : World.ScreenSpace -> String
createTranslateValue (World.ScreenSpace coordinate) =
    let
        { x, y } =
            calculateTranslateCoordinates { nativeX = coordinate.x, nativeY = coordinate.y }
    in
    "translate(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


{-|

  - `calculateTranslateCoordinates`

  - Translates given x and y coordinates from the World Module to the rendered Hex Grid coordinates.

  - The calculated Hex Grid coordinates are absolute specific to the rendered SVG Properties (which uses `translate` attribute).

-}
calculateTranslateCoordinates : { nativeX : Int, nativeY : Int } -> { x : Int, y : Int }
calculateTranslateCoordinates { nativeX, nativeY } =
    let
        x =
            if nativeX == 0 then
                20

            else
                20 + (nativeX * 30)

        y =
            if modBy 2 nativeX == 1 then
                38 + (nativeY * 36)

            else
                20 + (nativeY * 36)
    in
    { x = x, y = y }


chooseBiomeText : Maybe World.Biome -> String
chooseBiomeText biome =
    case biome of
        Just (World.Forest (World.MixedForest _) _ _ _) ->
            "MixedForest"

        Just (World.Plane World.MixedPlane _ _ _) ->
            "MixedPlane"

        Just (World.Forest (World.DarkForest _) _ _ _) ->
            "DarkForest"

        Just (World.Plane (World.RiverPlane _) _ _ _) ->
            "RiverPlane"

        Just (World.Forest World.RiverForest _ _ _) ->
            "RiverForest"

        Just (World.Rock World.GreyRock _ _ _) ->
            "GreyRock"

        Just (World.Rock World.DarkRock _ _ _) ->
            "DarkRock"

        Just (World.Forest World.MagicForest _ _ _) ->
            "MagicForest"

        Just (World.Forest World.LivingForest _ _ _) ->
            "LivingForest"

        Just (World.Forest World.DeepForest _ _ _) ->
            "DeepForest"

        Just (World.Lake World.WaterLake _ _ _) ->
            "WaterLake"

        Just (World.Rock World.RiverRock _ _ _) ->
            "RiverRock"

        Just (World.Plane World.MagicPlane _ _ _) ->
            "MagicPlane"

        Just (World.Forest World.DreamForest _ _ _) ->
            "DreamForest"

        Just (World.River World.WaterRiver _ _ _) ->
            "WaterRiver"

        Just (World.Ocean World.SaltyWaterOcean _ _ _) ->
            "SaltyWaterOcean"

        _ ->
            "Implement"
