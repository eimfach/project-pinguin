module Tests exposing (..)

import Expect
import Fuzz
import List.Nonempty
import Test exposing (..)
import World exposing (..)
import World.EcoSystemTypeDict



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


worldSuite : Test.Test
worldSuite =
    describe "The World Module"
        [ {- describe "World.getEcoSystemBiomeSeedingProperties"
             [ -- SHARE --
               -- share of regular biomes --
               test "Share of regular biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 36 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType RegularOccurrence)
             , test "Share of regular biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 72 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType RegularOccurrence)
             , test "Share of regular biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 144 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType RegularOccurrence)
             , test "Share of regular biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 288 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType RegularOccurrence)

             -- share of seldom biomes --
             , test "Share of seldom biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 12 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType SeldomOccurrence)
             , test "Share of seldom biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 24 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType SeldomOccurrence)
             , test "Share of seldom biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 48 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType SeldomOccurrence)
             , test "Share of seldom biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 96 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType SeldomOccurrence)

             -- share of rare biomes --
             , test "Share of rare biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 9 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType RareOccurrence)
             , test "Share of rare biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 18 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType RareOccurrence)
             , test "Share of rare biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 36 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType RareOccurrence)
             , test "Share of rare biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 72 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType RareOccurrence)

             -- share of unique biomes --
             , test "Share of unique biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 3 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType UniqueOccurrence)
             , test "Share of unique biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 6 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType UniqueOccurrence)
             , test "Share of unique biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 12 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType UniqueOccurrence)
             , test "Share of unique biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                 \_ ->
                     Expect.equal 24 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType UniqueOccurrence)
             ]
          -}
          describe "World.createGridViewPort"
            [ test "It should return a List of Coordinates which take 1/4 of the given coordinates from the center" <|
                \_ ->
                    Expect.equal
                        (Ok
                            [ { x = 3, y = 3 }
                            , { x = 4, y = 3 }
                            , { x = 3, y = 4 }
                            , { x = 4, y = 4 }
                            ]
                        )
                        (World.createGridViewPort
                            [ { x = 0, y = 0 }
                            , { x = 1, y = 0 }
                            , { x = 2, y = 0 }
                            , { x = 3, y = 0 }
                            , { x = 4, y = 0 }
                            , { x = 5, y = 0 }
                            , { x = 6, y = 0 }
                            , { x = 7, y = 0 }
                            , { x = 0, y = 1 }
                            , { x = 1, y = 1 }
                            , { x = 2, y = 1 }
                            , { x = 3, y = 1 }
                            , { x = 4, y = 1 }
                            , { x = 5, y = 1 }
                            , { x = 6, y = 1 }
                            , { x = 7, y = 1 }
                            , { x = 0, y = 2 }
                            , { x = 1, y = 2 }
                            , { x = 2, y = 2 }
                            , { x = 3, y = 2 }
                            , { x = 4, y = 2 }
                            , { x = 5, y = 2 }
                            , { x = 6, y = 2 }
                            , { x = 7, y = 2 }
                            , { x = 0, y = 3 }
                            , { x = 1, y = 3 }
                            , { x = 2, y = 3 }
                            , { x = 3, y = 3 }
                            , { x = 4, y = 3 }
                            , { x = 5, y = 3 }
                            , { x = 6, y = 3 }
                            , { x = 7, y = 3 }
                            , { x = 0, y = 4 }
                            , { x = 1, y = 4 }
                            , { x = 2, y = 4 }
                            , { x = 3, y = 4 }
                            , { x = 4, y = 4 }
                            , { x = 5, y = 4 }
                            , { x = 6, y = 4 }
                            , { x = 7, y = 4 }
                            , { x = 0, y = 5 }
                            , { x = 1, y = 5 }
                            , { x = 2, y = 5 }
                            , { x = 3, y = 5 }
                            , { x = 4, y = 5 }
                            , { x = 5, y = 5 }
                            , { x = 6, y = 5 }
                            , { x = 7, y = 5 }
                            , { x = 0, y = 6 }
                            , { x = 1, y = 6 }
                            , { x = 2, y = 6 }
                            , { x = 3, y = 6 }
                            , { x = 4, y = 6 }
                            , { x = 5, y = 6 }
                            , { x = 6, y = 6 }
                            , { x = 7, y = 6 }
                            , { x = 0, y = 7 }
                            , { x = 1, y = 7 }
                            , { x = 2, y = 7 }
                            , { x = 3, y = 7 }
                            , { x = 4, y = 7 }
                            , { x = 5, y = 7 }
                            , { x = 6, y = 7 }
                            , { x = 7, y = 7 }
                            ]
                        )
            ]
        ]


ecoSystemTypeDictSuite : Test.Test
ecoSystemTypeDictSuite =
    describe "The World.EcoSystemTypeDict Module"
        [ describe "World.EcoSystemTypeDict.construct"
            [ -- SHARE --
              -- share of regular biomes --
              test "Should return `Nothing` when a duplicate `World.EcoSystemType` is given." <|
                \_ ->
                    Expect.equal
                        Nothing
                        (World.EcoSystemTypeDict.construct
                            [ ( World.ModerateEcoSystemType, nonEmptyMixedForestBiomeList ), ( World.ModerateEcoSystemType, nonEmptyMixedForestBiomeList ) ]
                        )
            ]
        , describe "World.EcoSystemTypeDict.toList"
            [ test "Should return a `List (World.EcoSystemType, List.Nonempty.Nonempty World.Biome)`" <|
                \_ ->
                    case constructExpectedValidDict of
                        Just theDict ->
                            Expect.equal
                                [ ( World.ModerateEcoSystemType, nonEmptyMixedForestBiomeList )
                                , ( World.MoonEcoSystemType, nonEmptyMixedForestBiomeList )
                                ]
                                (World.EcoSystemTypeDict.toList theDict)

                        Nothing ->
                            constructionFail
            ]
        , describe "World.NonEmptyEcoSystemTypeDict.insert"
            [ test "Should return a new Dict with the updated biome list`" <|
                \_ ->
                    case constructExpectedValidDict of
                        Just theDict ->
                            let
                                anotherDict =
                                    World.EcoSystemTypeDict.construct
                                        [ ( World.MoonEcoSystemType, nonEmptyDarkForestBiomeList ), ( World.ModerateEcoSystemType, nonEmptyMixedForestBiomeList ) ]
                            in
                            case anotherDict of
                                Just expectedDict ->
                                    Expect.equal
                                        expectedDict
                                        (World.EcoSystemTypeDict.insert
                                            World.MoonEcoSystemType
                                            nonEmptyDarkForestBiomeList
                                            theDict
                                        )

                                Nothing ->
                                    constructionFail

                        Nothing ->
                            constructionFail
            ]
        , describe "World.EcoSystemTypeDict.get"
            [ test "Should just return the list of biomes if key exists`" <|
                \_ ->
                    case constructExpectedValidDict of
                        Just theDict ->
                            case World.EcoSystemTypeDict.get World.ModerateEcoSystemType theDict of
                                Just theBiomeList ->
                                    Expect.equal nonEmptyMixedForestBiomeList theBiomeList

                                Nothing ->
                                    Expect.fail "Expected to get `Just (List.Nonempty.Nonempty World.Biome)`, instead got `Nothing`"

                        Nothing ->
                            constructionFail
            , test "Should return `Nothing` if the key does not exist" <|
                \_ ->
                    case constructMissingKeyValidDict of
                        Just theDict ->
                            case World.EcoSystemTypeDict.get World.MoonEcoSystemType theDict of
                                Just _ ->
                                    Expect.fail "Expected to return Nothing, instead got `Just (List.Nonempty.Nonempty World.Biome)`"

                                Nothing ->
                                    Expect.pass

                        Nothing ->
                            constructionFail
            ]
        ]


constructionFail : Expect.Expectation
constructionFail =
    Expect.fail "Dict construction was `Nothing` but `Just (World.NonEmptyEcoSystemTypeDict.NonEmptyEcoSystemTypeDict) was expected."


constructExpectedValidDict : Maybe World.EcoSystemTypeDict.EcoSystemTypeDict
constructExpectedValidDict =
    World.EcoSystemTypeDict.construct
        [ ( World.ModerateEcoSystemType, nonEmptyMixedForestBiomeList ), ( World.MoonEcoSystemType, nonEmptyMixedForestBiomeList ) ]


constructMissingKeyValidDict : Maybe World.EcoSystemTypeDict.EcoSystemTypeDict
constructMissingKeyValidDict =
    World.EcoSystemTypeDict.construct
        [ ( World.ModerateEcoSystemType, nonEmptyMixedForestBiomeList ) ]


nonEmptyMixedForestBiomeList : List.Nonempty.Nonempty World.Biome
nonEmptyMixedForestBiomeList =
    List.Nonempty.Nonempty (World.Forest (World.MixedForest World.TwoSpread) World.AverageTemp World.MediumFertility World.MediumHydration) []


nonEmptyDarkForestBiomeList : List.Nonempty.Nonempty World.Biome
nonEmptyDarkForestBiomeList =
    List.Nonempty.Nonempty (World.Forest (World.DarkForest World.TwoSpread) World.AverageTemp World.MediumFertility World.MediumHydration) []
