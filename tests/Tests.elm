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
        [ describe "World.getEcoSystemBiomeSeedingProperties"
            [ -- SHARE --
              -- share of regular biomes --
              test "Share of regular biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 36 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType RegularOccurrence |> .share)
            , test "Share of regular biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 72 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType RegularOccurrence |> .share)
            , test "Share of regular biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 144 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType RegularOccurrence |> .share)
            , test "Share of regular biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 288 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType RegularOccurrence |> .share)

            -- share of seldom biomes --
            , test "Share of seldom biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 12 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType SeldomOccurrence |> .share)
            , test "Share of seldom biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 24 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType SeldomOccurrence |> .share)
            , test "Share of seldom biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 48 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType SeldomOccurrence |> .share)
            , test "Share of seldom biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 96 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType SeldomOccurrence |> .share)

            -- share of rare biomes --
            , test "Share of rare biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 9 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType RareOccurrence |> .share)
            , test "Share of rare biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 18 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType RareOccurrence |> .share)
            , test "Share of rare biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 36 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType RareOccurrence |> .share)
            , test "Share of rare biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 72 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType RareOccurrence |> .share)

            -- share of unique biomes --
            , test "Share of unique biomes in small EcoSystemSize (=60) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 3 (getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType UniqueOccurrence |> .share)
            , test "Share of unique biomes in medium EcoSystemSize (=120) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 6 (getEcoSystemBiomeSeedingProperties MediumEcoSystem ModerateEcoSystemType UniqueOccurrence |> .share)
            , test "Share of unique biomes in large EcoSystemSize (=240) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 12 (getEcoSystemBiomeSeedingProperties LargeEcoSystem ModerateEcoSystemType UniqueOccurrence |> .share)
            , test "Share of unique biomes in huge EcoSystemSize (=480) and moderate EcosystemType" <|
                \_ ->
                    Expect.equal 24 (getEcoSystemBiomeSeedingProperties HugeEcoSystem ModerateEcoSystemType UniqueOccurrence |> .share)
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
