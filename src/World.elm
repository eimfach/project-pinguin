module World exposing
    ( Biome(..)
    , BiomeSpread(..)
    , Chunk
    , DesertBiome(..)
    , EcoSystemSize(..)
    , EcoSystemType(..)
    , Fertility(..)
    , ForestBiome(..)
    , Hydration(..)
    , IceBiome(..)
    , LakeBiome(..)
    , LavaBiome(..)
    , Occurrence(..)
    , OceanBiome(..)
    , PlaneBiome(..)
    , RiverBiome(..)
    , RockBiome(..)
    , Temperature(..)
    , addLandmassDistribution
    , createWorldMapGrid
    , getEcoSystemBiomeSeedingProperties
    )

import List.Nonempty



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
---------------------------------------- WORLD GENERATION ----------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************
{-
    General
    - A WorldMap has six Ecosystems
    - The `type EcoSystemSize` has a size (Small, Medium, Large, Huge) which tells how many `type Chunk` /Hex Fields will be in there
    - A function receiving the `type EcoSystemSize` returns an Integer (60, 120, 240, 480) (`translateEcoSystemSize`)
      example: an EcosystemSize of type Large has 240 Chunks
    - Every Ecosystem has the same size on generation

    ## Generation
    ### First Step -> Generate Landmass Biomes
    - First every Ecosystem is generated step by step
    - Every Ecosystem has a type `type EcosystemType`
    - Now for every Ecosystem multiple Lists of random Biomes is created
    - Every List represents Biomes of every Occurrence (`type Occurrence`)
    - A function receiving `type Occurrence` and the `type EcosystemType`
      returns a List of possible Biomes for every Occurrence (called BiomeSeedList)
    - The Share tells how large the generated list will be
    - Every Occurrence reflects the Share in EcoSystemSize
    - Occurrences:
     type Occurrence
         = RegularOccurrence (60% share of EcoSystemSize)
         | SeldomOccurrence (20% share of EcoSystemSize)
         | RareOccurrence (15%  share of EcoSystemSize)
         | UniqueOccurrence (5% share of EcoSystemSize)
   - The generated Biome Lists are then merged into one List and this List is mixed randomly

    - *This World Module exposes a function (`getEcoSystemBiomeSeedingProperties`) which is receiving `type Occurrence`, `type EcosystemType` and `type EcoSystemSize`
      and returns a List of possible Biomes for every Occurrence, and the Share as an Int.*
      It represents the first step into World Generation

    - The random generation of Biomes works as following
    - For every generated List a dice is rolled where face count equals the length of the BiomeSeedList
    - The dice result then will be used as an index to get a Biome from the BiomeSeedList. This Biome will be added to the generated List.

    ### Second Step -> Generate Ocean Biomes and World Map Grid
    1. Create a basic world map grid with ocean biomes
    - An exposed function `createWorldMapGrid` takes `EcoSystemSize`, calls `translateEcoSystemSize` multiplies the result with 24
    and returns a List of `Chunk` with `Ocean` Biomes and calculated Coordinates
    (1/4 Landmass; 3/4 Ocean)
    2. An exposed function `addLandmassDistribution` takes a List of `Chunk` (the WorldMapGrid),
    a `List (List Biome)` of Landmass Biomes (the generated Biomes from the first step) and a custom type `LandmassDistribution`
    and returns a new List of `Chunk` with the landmass biomes inserted according to the `LandmassDistribution`



-}
{-
   - A Coordinate represents one hex in x and y axis system, it lives in a `Chunk`
-}


type LandMassDistribution
    = Continents ContinentAmount


type ContinentAmount
    = TwoContinents


addLandmassDistribution : LandMassDistribution -> List Chunk -> List (List Biome) -> List Int -> List Chunk
addLandmassDistribution distribution worldMapGrid biomes randomList =
    worldMapGrid


createWorldMapGrid : EcoSystemSize -> List Chunk
createWorldMapGrid ecoSystemSize =
    let
        baseSize =
            translateEcoSystemSize ecoSystemSize
    in
    List.indexedMap (createBasicOceanChunk ecoSystemSize) <|
        List.repeat
            (floor <| baseSize * 50)
            (Ocean SaltyWaterOcean AverageTemp MediumFertility MediumHydration)


createBasicOceanChunk : EcoSystemSize -> Int -> Biome -> Chunk
createBasicOceanChunk ecoSystemSize index biome =
    Chunk
        (calculateCoordinates (floor <| translateEcoSystemSize ecoSystemSize) index)
        LayerConnectionEnd
        biome


calculateCoordinates : Int -> Int -> Coordinate
calculateCoordinates rowSize index =
    let
        y =
            index // rowSize

        x =
            index - (y * rowSize)
    in
    Coordinate x y


type alias Coordinate =
    { x : Int
    , y : Int
    }



{-
   - [x] A Chunk is a single in game field
   - [x] A Chunk has a Coordinate, x and y as Integer
   - [x] A Chunk always has four layers, from top to bottom: Atmosphere, Ground, Underground, Deep Underground
   - [x] A Chunk has an associated biome
   - [ ] A Chunk has a natural growth rate
-}


type alias Chunk =
    { coordinate : Coordinate
    , layers : LayerConnector
    , biome : Biome
    }



{-
   - A Layer has a collection of entities like resources, characters, structures, weather ...
   - A Layer has environmental properties like material class (stone, sand, rock...)
     or possible resources, flora class and flora states like water supply which leads to grow rate
   - A Layer can have multiple magical effects on it (like magic hydration or alteration of flora class)
   - Layers can be permanently altered after some time applying magic to it:
    -> like changing the weather or, with mighty magic, even the BaseMaterialClass or biome, for example:
        -> You can create lakes or rivers, enchant forests, create forest illusions, soften rock for easier mining,
           curse forests or improve fertility/hydration or just cast rain, change the temperature,
           create forests in deserts (but desertification)
        -> Note: You can curse forests around you, to build a defensive wall, and they won't change appearance.
           However, cursed trees can't be chopped until the curse is removed. There can be a lot of ways to curse a forest.
        -> Or you can apply other magical effects onto a layer, like a magical shield around your village where nobody can get in or out for some time.
-}


type Layer
    = Atmosphere (List MagicEffects) (List WeatherEffects)
    | Ground BaseMaterialClass FloraState GrowthRate (List MagicEffects) (List NaturalEffects) (List Entity)
    | Underground BaseMaterialClass FloraState GrowthRate (List MagicEffects) (List NaturalEffects) (List Entity)
    | DeepUnderground BaseMaterialClass (List MagicEffects) (List NaturalEffects) (List Entity)



{-
   A LayerConnector tells the order of layers
-}


type LayerConnector
    = LayerConnector ( Layer, LayerConnector )
    | LayerConnectionEnd



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
--------------------------------------------- GENERAL --------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


type Occurrence
    = RegularOccurrence
    | SeldomOccurrence
    | RareOccurrence
    | UniqueOccurrence


type Complexity
    = EyeBlink
    | VeryLow
    | Low
    | Simple
    | Average
    | Ambitious
    | Hard
    | VeryHard
    | Godlike


type EasyProgressScale
    = FirstEasyProgressPoint
    | LastEasyProgressPoint


type AverageProgressScale
    = FirstAverageProgressPoint
    | SecondAverageProgressPoint
    | ThirdAverageProgressPoint
    | FourthAverageProgressPoint
    | LastAverageProgressPoint


type DefaultProgressScale
    = FirstDefaultProgressPoint
    | SecondDefaultProgressPoint
    | ThirdDefaultProgressPoint
    | FourthDefaultProgressPoint
    | FifthDefaultProgressPoint
    | SixthDefaultProgressPoint
    | SeventhDefaultProgressPoint
    | EighthDefaultProgressPoint
    | LastDefaultProgressPoint


type HardProgressScale
    = FirstHardProgressPoint
    | SecondHardProgressPoint
    | ThirdHardProgressPoint
    | FourthHardProgressPoint
    | FifthHardProgressPoint
    | SixthHardProgressPoint
    | SeventhHardProgressPoint
    | EighthHardProgressPoint
    | NinthHardProgressPoint
    | TenthHardProgressPoint
    | EleventhHardProgressPoint
    | TwelfthHardProgressPoint
    | ThirteenthHardProgressPoint
    | FourteenthHardProgressPoint
    | FifteenthHardProgressPoint
    | SixteenthHardProgressPoint
    | SeventeenthHardProgressPoint
    | EighteenthHardProgressPoint
    | LastHardProgressPoint


type PainfulProgressScale
    = FirstPainfulProgressPoint
    | SecondPainfulProgressPoint
    | ThirdPainfulProgressPoint
    | FourthPainfulProgressPoint
    | FifthPainfulProgressPoint
    | SixthPainfulProgressPoint
    | SeventhPainfulProgressPoint
    | EighthPainfulProgressPoint
    | NinthPainfulProgressPoint
    | TenthPainfulProgressPoint
    | EleventhPainfulProgressPoint
    | TwelfthPainfulProgressPoint
    | ThirteenthPainfulProgressPoint
    | FourteenthPainfulProgressPoint
    | FifteenthPainfulProgressPoint
    | SixteenthPainfulProgressPoint
    | SeventeenthPainfulProgressPoint
    | EighteenthPainfulProgressPoint
    | NineteenthPainfulProgressPoint
    | TwentiethPainfulProgressPoint
    | TwentyFirstPainfulProgressPoint
    | TwentySecondPainfulProgressPoint
    | TwentyThirdPainfulProgressPoint
    | TwentyFourthPainfulProgressPoint
    | TwentyFifthPainfulProgressPoint
    | TwentySixthPainfulProgressPoint
    | TwentySeventhPainfulProgressPoint
    | TwentyEighthPainfulProgressPoint
    | LastPainfulProgressPoint



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
----------------------------------------- BIOMES & NATURE ----------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************
{-
   - Biome
       - A Biome has a name which reflects something like the weather, monsters, environment or resources
       - A Biome has a weather type associated
       - A Biome has Temperature associated
       - A 'Blood' Biome can only be create by a sacrifice ritual - sacrifice intelligent life
       - An 'Illusion' Biome can hide the real Biome, it can be used for a trap (Have forest illusion but the real biome is lava)
       - Enchanted Biome can bless the group
       - An Artificial Biome is a Biome which was permanently altered
       - A Magic Biome is a rare found which has special events
       - A cursed Biome can curse groups standing on it
       - Artificial ColdLava Biomes can not be created, only FluidLava ones, you can then wait until the lava cools down or cast magic on it to cool it down
-}


type Biome
    = Plane PlaneBiome Temperature Fertility Hydration
    | Rock RockBiome Temperature Fertility Hydration
    | Ice IceBiome Temperature Fertility Hydration
    | Forest ForestBiome Temperature Fertility Hydration
    | Lava LavaBiome Temperature Fertility Hydration
    | Lake LakeBiome Temperature Fertility Hydration
    | River RiverBiome Temperature Fertility Hydration
    | Ocean OceanBiome Temperature Fertility Hydration
    | Desert DesertBiome Temperature Fertility Hydration
    | Mountain MountainBiome Temperature Fertility Hydration
    | Artificial ArtificialBiome Temperature Fertility Hydration



{-
   - BiomeSpread
        - means that the Biome does spread over multiple connected hexes consuming others (for creating larger connected forest or mountains)
-}


type BiomeSpread
    = NoSpread
    | OneSpread
    | TwoSpread
    | ThreeSpread
    | FourSpread
    | FiveSpread
    | TenSpread


type ForestBiome
    = MixedForest BiomeSpread
    | LeafyForest
    | ConiferForest
    | DeepForest
    | DarkForest BiomeSpread
    | DeepDarkForest
    | RiverForest
    | LivingForest
    | BloodForest
    | DreamForest
    | RainForest
    | IceForest
    | MagicForest
    | HillMixedForest
    | EverGreenForest


type PlaneBiome
    = MixedPlane
    | DryPlane
    | DarkPlane
    | DarkMixedPlane
    | RiverPlane BiomeSpread -- Has Biome Spread
    | DesertPlane
    | DarkDesertPlane
    | BloodDesertPlane
    | MagicPlane


type RockBiome
    = GreyRock
    | DarkRock
    | BloodRock
    | RiverRock
    | MoonRock
    | DesertRock
    | DesertDarkRock
    | MagicRock
    | HillGreyRock


type IceBiome
    = WhiteIce
    | BlackIce
    | BloodIce
    | DesertIce
    | RockIce
    | MagicIce
    | HillIce
    | MoonIce


type LavaBiome
    = FluidLava -- looks redish
    | MagicFluidLava -- maybe looks purple ? looses it's magic properties when it gets cold
    | ColdRockLava
    | BloodColdRockLava


type LakeBiome
    = WaterLake -- Has BiomeSpread
    | MoonLake
    | Oasis
    | MoonOasis
    | DarkOasis
    | BloodLake


type RiverBiome
    = WaterRiver -- Has BiomeSpread


type OceanBiome
    = SaltyWaterOcean


type DesertBiome
    = SandDesert
    | DarkSandDesert
    | BloodDesert
    | LostDesert
    | HillDesert


type MountainBiome
    = SnowMountain
    | IceMountain
    | DarkMountain
    | MagicMountain
    | DeepCaveMountain
    | Canyon



{-
   - ArtificialBiome
        - replaces a chunks regular Biome via TerraForm spell, which means it can't be undone or treated as magical effect.
        Only Weather can change the Biome after it. But you can control Weather :P
-}


type ArtificialBiome
    = ArtificialForest ForestBiome
    | ArtificialPlane PlaneBiome
    | ArtificialRock RockBiome
    | ArtificialIce IceBiome
    | ArtificialLava LavaBiome
    | ArtificialLake LakeBiome
    | ArtificialRiver RiverBiome
    | ArtificialOcean OceanBiome
    | ArtificialDesert DesertBiome



{-
   IllusionBiome is only possible as magical effect
-}


type IllusionBiome
    = IllusionForest ForestBiome
    | IllusionPlane PlaneBiome
    | IllusionRock RockBiome
    | IllusionIce IceBiome
    | IllusionLava LavaBiome
    | IllusionLake LakeBiome
    | IllusionRiver RiverBiome
    | IllusionOcean OceanBiome
    | IllusionDesert DesertBiome



{-
   EnchantedBiome is only possible as magical effect
-}


type EnchantedBiome
    = EnchantedForest ForestBiome
    | EnchantedPlane PlaneBiome
    | EnchantedLake LakeBiome



{-
   CursedBiome is only possible as magical effect
-}


type CursedBiome
    = CursedForest ForestBiome
    | CursedDesert DesertBiome
    | CursedMountain MountainBiome
    | CursedLake LakeBiome



{-
   - WeatherEffects
        - EternalSnowStorm is deadly and permanent
-}


type WeatherEffects
    = Sunny Temperature
    | Clear Temperature
    | Clouded Temperature
    | LightRain Temperature
    | MediumRain Temperature
    | HeavyRain Temperature
    | Monsoon Temperature
    | BloodRain Temperature
    | LightSnow Temperature
    | HeavySnow Temperature
    | SnowStorm Temperature
    | EternalSnowStorm
    | Storm Temperature
    | HeavyStorm Temperature
    | ThunderStorm Temperature


type Temperature
    = MeltingHot
    | VeryHot
    | Hot
    | Warm
    | AverageTemp
    | Mild
    | Cold
    | VeryCold
    | IceCold


type BaseMaterialClass
    = SoftRock
    | MediumRock
    | HardRock
    | SoftSand
    | MediumSand
    | HardSand
    | Gravel
    | Soil
    | Water


type Fertility
    = NoFertility
    | LowFertility
    | MediumFertility
    | HighFertility
    | PerfectFertility


type Hydration
    = Dehydrated
    | LowHydration
    | MediumHydration
    | HighHydration
    | PerfectHydration
    | OverHydrated


type alias FloraState =
    { hydration : Hydration
    , fertility : Fertility
    }


type GrowthRate
    = Decay
    | NoGrowth EasyProgressScale
    | VeryLowGrowth EasyProgressScale
    | LowGrowth AverageProgressScale
    | AverageGrowth AverageProgressScale
    | AcceptableGrowth DefaultProgressScale
    | SubstantialGrowth HardProgressScale
    | EnormousGrowth PainfulProgressScale
    | MassiveGrowth
    | OverGrowth -- Only by magical effect ?


getMaxGrowthRate : Fertility -> GrowthRate
getMaxGrowthRate fertility =
    case fertility of
        NoFertility ->
            NoGrowth FirstEasyProgressPoint

        LowFertility ->
            LowGrowth LastAverageProgressPoint

        MediumFertility ->
            AcceptableGrowth LastDefaultProgressPoint

        HighFertility ->
            EnormousGrowth LastPainfulProgressPoint

        PerfectFertility ->
            MassiveGrowth


type NaturalEffects
    = PlantSeedlings


type NaturalSkills
    = Reproduce


type FloraEntity
    = Tree TreeSize (List NaturalSkills)



-- type TreeType


type TreeSize
    = Seedling
    | Sapling



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
-------------------------------------------- ECOSYSTEMS ------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


type EcoSystemType
    = ModerateEcoSystemType
    | MoonEcoSystemType


type EcoSystemSize
    = SmallEcoSystem
    | MediumEcoSystem
    | LargeEcoSystem
    | HugeEcoSystem



{-
   - EcoSystemSeedingProperties
        - A record holding data for generating ecosystem biomes
        - seedList : A list of possible biomes, you can take rolls on it
        - share: how many of the given biome types in seedList will be in the ecosystem ?
        - dice faces should equal the list length - 1, then pick from seedList by rolled index
-}


type alias EcoSystemSeedingProperties =
    { seedList : List.Nonempty.Nonempty Biome
    , share : Int
    }


translateEcoSystemSize : EcoSystemSize -> Float
translateEcoSystemSize ecoSystemSize =
    case ecoSystemSize of
        SmallEcoSystem ->
            60.0

        MediumEcoSystem ->
            120.0

        LargeEcoSystem ->
            240.0

        HugeEcoSystem ->
            480.0


calculateBiomeOccurrenceAmount : EcoSystemSize -> Occurrence -> Int
calculateBiomeOccurrenceAmount ecoSystemSize occurrence =
    case occurrence of
        RegularOccurrence ->
            floor <| translateEcoSystemSize ecoSystemSize * 0.6

        SeldomOccurrence ->
            floor <| translateEcoSystemSize ecoSystemSize * 0.2

        RareOccurrence ->
            floor <| translateEcoSystemSize ecoSystemSize * 0.15

        UniqueOccurrence ->
            floor <| translateEcoSystemSize ecoSystemSize * 0.05


getEcoSystemBiomeSeedingProperties : EcoSystemSize -> EcoSystemType -> Occurrence -> EcoSystemSeedingProperties
getEcoSystemBiomeSeedingProperties ecoSystemSize ecoSystemType occurrence =
    case ecoSystemType of
        ModerateEcoSystemType ->
            { seedList = getModerateEcoSystemBiomeSeedList occurrence
            , share = calculateBiomeOccurrenceAmount ecoSystemSize occurrence
            }

        MoonEcoSystemType ->
            { seedList = getMoonEcoSystemBiomeSeedList occurrence
            , share = calculateBiomeOccurrenceAmount ecoSystemSize occurrence
            }



{-
   - Moderate Ecosystem:
       - Fertility: Average - High
       - Temperature: Average
       - Hydration: Medium - High
       - Weather : ?
       - Resources: Average - High | Stone and Wood

       The Moderate Ecosystem is dominated by rich mostly mixed and green forests, wide planes and average climate.
       You can find rocky landscapes here too.
       The soil is rich and very fertile, the weather provides enough hydration, so surviving here should not be hard.
       You will find some magic forests or planes. You can find a lot of wood and stone resources here.
-}


getModerateEcoSystemBiomeSeedList : Occurrence -> List.Nonempty.Nonempty Biome
getModerateEcoSystemBiomeSeedList occurrence =
    case occurrence of
        RegularOccurrence ->
            -- RegularSeedList --
            List.Nonempty.Nonempty
                (Plane MixedPlane AverageTemp HighFertility HighHydration)
                [ Forest (MixedForest ThreeSpread) AverageTemp HighFertility MediumHydration
                , Plane (RiverPlane FourSpread) AverageTemp HighFertility HighHydration
                , Forest (MixedForest ThreeSpread) AverageTemp HighFertility MediumHydration
                , Rock GreyRock AverageTemp LowFertility LowHydration
                , Forest (MixedForest ThreeSpread) Warm MediumFertility MediumHydration
                , Plane MixedPlane Warm MediumFertility LowHydration
                , Lake WaterLake AverageTemp MediumFertility HighHydration
                , Plane MixedPlane AverageTemp LowFertility LowHydration
                , Forest HillMixedForest Warm MediumFertility MediumHydration
                , Forest RiverForest AverageTemp HighFertility MediumHydration
                , Rock GreyRock AverageTemp LowFertility LowHydration
                , Plane MixedPlane AverageTemp HighFertility HighHydration
                , Forest (DarkForest OneSpread) Cold MediumFertility LowHydration
                , Lake WaterLake AverageTemp MediumFertility HighHydration
                ]

        SeldomOccurrence ->
            -- SeldomSeedList --
            List.Nonempty.Nonempty
                (Forest DeepForest Mild MediumFertility LowHydration)
                [ Forest (DarkForest OneSpread) Cold MediumFertility MediumHydration
                , Forest DeepForest Mild MediumFertility LowHydration
                , Lake WaterLake Cold LowFertility HighHydration
                , Forest (DarkForest TwoSpread) Cold LowFertility MediumHydration
                , Forest (MixedForest FourSpread) AverageTemp HighFertility MediumHydration
                , Rock GreyRock AverageTemp LowFertility LowHydration
                , Lake WaterLake AverageTemp HighFertility HighHydration
                , Plane MixedPlane Warm HighFertility MediumHydration
                , Rock GreyRock Cold LowFertility LowHydration
                , Forest (MixedForest FourSpread) AverageTemp HighFertility MediumHydration
                , Forest (MixedForest TwoSpread) Warm HighFertility HighHydration
                , Lake WaterLake AverageTemp HighFertility HighHydration
                , Rock GreyRock AverageTemp LowFertility LowHydration
                , Forest HillMixedForest Warm MediumFertility MediumHydration
                , Forest (MixedForest NoSpread) Warm LowFertility Dehydrated -- Rotten Forest
                ]

        RareOccurrence ->
            -- RareSeedList --
            List.Nonempty.Nonempty
                (Plane DarkMixedPlane Mild MediumFertility LowHydration)
                [ Plane (RiverPlane FiveSpread) Warm HighFertility HighHydration
                , Forest RiverForest Warm HighFertility HighHydration
                , Forest (MixedForest NoSpread) AverageTemp HighFertility MediumHydration
                , Plane DryPlane Mild LowFertility Dehydrated
                , Rock GreyRock Warm LowFertility HighHydration
                , Lake WaterLake AverageTemp HighFertility HighHydration
                , Forest HillMixedForest AverageTemp HighFertility MediumHydration
                , Lake WaterLake Cold LowFertility HighHydration
                , River WaterRiver Cold LowFertility HighHydration
                , Mountain SnowMountain VeryCold LowFertility MediumHydration
                , Forest (MixedForest OneSpread) AverageTemp HighFertility MediumHydration
                , Rock DarkRock AverageTemp LowFertility Dehydrated
                , Rock HillGreyRock AverageTemp LowFertility LowHydration
                , Rock RiverRock AverageTemp LowFertility LowHydration
                , Forest (MixedForest OneSpread) Cold MediumFertility OverHydrated
                ]

        UniqueOccurrence ->
            List.Nonempty.Nonempty
                (Forest MagicForest Warm HighFertility HighHydration)
                [ Forest LivingForest Warm NoFertility MediumHydration
                , Forest (MixedForest NoSpread) AverageTemp HighFertility MediumHydration
                , Forest (DarkForest ThreeSpread) AverageTemp HighFertility MediumHydration
                , Rock DarkRock AverageTemp MediumFertility MediumHydration
                , Plane MagicPlane Cold MediumFertility LowHydration
                , Mountain SnowMountain VeryCold LowFertility MediumHydration
                , Forest (MixedForest FiveSpread) Warm HighFertility MediumHydration
                , Forest DreamForest VeryCold NoFertility LowHydration
                , Lake WaterLake Warm HighFertility HighHydration
                , Rock DarkRock AverageTemp MediumFertility MediumHydration
                , Forest (DarkForest OneSpread) AverageTemp HighFertility HighHydration
                , Plane DarkMixedPlane Mild HighFertility MediumHydration
                , Forest (MixedForest NoSpread) Warm LowFertility Dehydrated -- Rotten Forest - Maybe Cursed ?
                ]



{-
   Moon Ecosystem:
       - Fertility: Low
       - Temperature: Cold
       - Hydration: Medium
       - Resources: Very High | Gem and Metal
       - Weather: ?

       The Moon Ecosystem is dominated by mountains, caves, dark and deep forests, moon lakes, dark planes and cold climate.
       The soil is purple dark, dry and less fertile, the weather average and snowy sometimes. Surviving here is not too easy.
       However the landscapes are full of valuable resources.
       You can find a lot of wood, stone, gems and metal and mushroom resources here.
-}


getMoonEcoSystemBiomeSeedList : Occurrence -> List.Nonempty.Nonempty Biome
getMoonEcoSystemBiomeSeedList occurrence =
    case occurrence of
        RegularOccurrence ->
            List.Nonempty.Nonempty
                (Mountain SnowMountain Cold NoFertility LowHydration)
                [ Mountain SnowMountain Cold LowFertility LowHydration
                , Forest (DarkForest TwoSpread) Cold LowFertility LowHydration
                , Plane DarkPlane Cold LowFertility MediumHydration
                , Forest DeepForest AverageTemp LowFertility LowHydration
                , Forest ConiferForest Cold LowFertility LowHydration
                , Lake WaterLake Cold MediumFertility MediumHydration
                , Mountain SnowMountain Cold NoFertility LowHydration
                , Rock GreyRock Cold NoFertility LowHydration
                , Forest (DarkForest ThreeSpread) Cold LowFertility LowHydration
                , Plane DarkPlane Cold LowFertility MediumHydration
                , Forest ConiferForest Cold LowFertility LowHydration
                , Lake WaterLake Cold LowFertility MediumHydration
                , Rock GreyRock Cold NoFertility MediumHydration
                , Plane DarkPlane Cold LowFertility MediumHydration
                ]

        SeldomOccurrence ->
            List.Nonempty.Nonempty
                (Mountain DarkMountain VeryCold NoFertility Dehydrated)
                [ Lake MoonLake Cold LowFertility HighHydration
                , Forest (DarkForest FourSpread) Cold LowFertility LowHydration
                , Forest ConiferForest Cold LowFertility MediumHydration
                , Rock GreyRock Cold LowFertility MediumHydration
                , Rock DarkRock Cold LowFertility LowHydration
                , Mountain SnowMountain Cold LowFertility LowHydration
                , Lake MoonLake VeryCold MediumFertility HighHydration
                , Forest RiverForest Cold MediumFertility HighHydration
                , Forest DeepForest Cold LowFertility LowHydration
                , Mountain SnowMountain Cold LowFertility LowHydration
                , Plane DarkPlane AverageTemp MediumFertility LowHydration
                , Plane DarkMixedPlane Cold LowFertility LowHydration
                , Forest (DarkForest ThreeSpread) AverageTemp LowFertility MediumHydration
                , Plane DarkPlane Cold MediumFertility MediumHydration
                , Mountain DeepCaveMountain Cold LowFertility LowHydration
                , Plane DryPlane AverageTemp NoFertility Dehydrated
                , Mountain DeepCaveMountain Cold MediumFertility MediumHydration
                ]

        RareOccurrence ->
            List.Nonempty.Nonempty
                (Lake MoonLake Cold MediumFertility HighHydration)
                [ Mountain DeepCaveMountain Cold HighFertility MediumHydration
                , Plane DarkMixedPlane Cold MediumFertility LowHydration
                , Forest ConiferForest Cold MediumFertility MediumHydration
                , Rock MoonRock Cold LowFertility LowHydration
                , Forest DeepForest Cold LowFertility LowHydration
                , Lake MoonLake Cold MediumFertility HighHydration
                , Mountain DeepCaveMountain Cold LowFertility LowHydration
                , Forest DeepDarkForest Cold LowFertility MediumHydration
                , Forest DreamForest Cold LowFertility MediumHydration
                , Rock HillGreyRock Cold LowFertility LowHydration
                , Mountain SnowMountain Cold MediumFertility MediumHydration
                , Rock MoonRock Cold LowFertility LowHydration
                , Lake MoonOasis AverageTemp HighFertility MediumHydration
                ]

        UniqueOccurrence ->
            List.Nonempty.Nonempty
                (Lake MoonLake Cold MediumFertility HighHydration)
                [ Mountain DeepCaveMountain Cold HighFertility MediumHydration
                , Forest DeepDarkForest Cold MediumFertility MediumHydration
                , Forest MagicForest Cold MediumFertility MediumHydration
                , Forest ConiferForest AverageTemp HighFertility MediumHydration
                , Mountain MagicMountain Cold LowFertility MediumHydration
                , Mountain Canyon Cold LowFertility LowHydration
                , Mountain DarkMountain Cold LowFertility MediumHydration
                , Mountain DeepCaveMountain Cold MediumFertility HighHydration
                ]



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
------------------------------------- RESOURCES AND CRAFT MATERIAL -------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


type CraftMaterialClass
    = WoodMaterialClass WoodResource
    | StoneMaterialClass
    | GemMaterialClass GemResource
    | BoneMaterialClass
    | MetalMaterialClass
    | LeatherMaterialClass


createComposite : CraftMaterialClass -> CraftMaterialClass -> CompositeResource
createComposite firstMaterial secondMaterial =
    case ( firstMaterial, secondMaterial ) of
        ( WoodMaterialClass AncientWood, GemMaterialClass Diamond ) ->
            CrystalWood

        _ ->
            CrystalWood


type CompositeResource
    = CrystalWood


type GemResource
    = Amber
    | Topaz
    | Malachite
    | Ruby
    | Diamond


type WoodResource
    = Wood
    | ElvenWood
    | DryadWood
    | DarkWood
    | AncientWood



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
--------------------------------------------- MAGIC ----------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************
{-
   - Magic Energy drains from the Magic caster like fluid. It can be cast without materials but it weakens the caster, like dehydrating
   his life essence. A Caster can sacrifice himself this way to cast a very powerful spell.
-}


type MagicEnergy
    = OneDrop
    | CoupleDrops
    | TenDrops
    | DozensOfDrops
    | HundredsOfDrops
    | ThousandsOfDrops
    | SmallLakeOfDrops
    | HugeLakeOfDrops -- will most likely kill the caster
    | OceanOfDrops -- will kill the caster


type MagicEffects
    = Fog
    | Poison
    | Shield
    | Hydration
    | Fertility
    | Temperature
    | Weather WeatherEffects
    | Curse Entity Curse
    | CursedBiome CursedBiome Curse
    | IllusionBiome IllusionBiome Curse
    | EnchantedBiome EnchantedBiome Spell
    | TerraForming


type Curse
    = ToDo


type Spell
    = IncreaseHydration
    | IncreaseFertility
    | TerraForm


type SpellResources
    = Feather


type MagicShool
    = Terra -- Magic Shool which does focus on altering the natural enviroment



-- calculateSpellComplexity :
-- getSpellCost : Spell -> Complexity -> { spellResources : List SpellResources, magicEnergy : MagicEnergy }
-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
-------------------------------------------- ENTITIES --------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


type Entity
    = Resource
    | PC Race CharacterClass MagicEffects
    | NPC Race CharacterClass MagicEffects
    | Flora FloraEntity



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
---------------------------------------- CHARACTERS & RACES --------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


type Race
    = Human (List NaturalSkills)
    | Dwarf (List NaturalSkills)
    | Elf (List NaturalSkills)


type CharacterClass
    = Fighter
    | Hunter
    | Gatherer
    | Craftsmen
    | Healer
    | Sorcerer



-- ******************************************************************************************************
--------------------------------------------------------------------------------------------------------|
----------------------------------------------- EVENTS -------------------------------------------------|
--------------------------------------------------------------------------------------------------------|
-- ******************************************************************************************************


type Event
    = Environment
