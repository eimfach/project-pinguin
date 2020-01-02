module Tests exposing (..)

import Expect
import List.Nonempty
import Test exposing (..)
import World exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


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


getAmountOfOneRegularBiomeInModerateEcoSystemFromSeedList : Biome -> Int
getAmountOfOneRegularBiomeInModerateEcoSystemFromSeedList =
    getAmountOfOneBiomeInSeedList SmallEcoSystem ModerateEcoSystemType RegularOccurrence


getAmountOfOneBiomeInSeedList : EcoSystemSize -> EcoSystemType -> Occurrence -> Biome -> Int
getAmountOfOneBiomeInSeedList ecoSystemSize ecoSystemType occurrence biome =
    getEcoSystemBiomeSeedingProperties SmallEcoSystem ModerateEcoSystemType RegularOccurrence
        |> .seedList
        |> filterBiome biome
        |> List.length


filterBiome : Biome -> List.Nonempty.Nonempty Biome -> List Biome
filterBiome biome biomeSeedList =
    List.filter (\currentBiome -> currentBiome == biome) (List.Nonempty.toList biomeSeedList)
