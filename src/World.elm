module World exposing
    ( Biome(..)
    , BiomeSpread(..)
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
    , getEcoSystemBiomeSeedingProperties
    )

import List.Nonempty



{-
    World generation

    General
    - A WorldMap has multiple Ecosystems
    - The `type EcoSystemSize` has a size (Small, Medium, Large, Huge) which tells how many `type Chunk` /Hex Fields will be in there
    - A function receiving the `type EcoSystemSize` returns an Integer (60, 120, 240, 480) (`translateEcoSystemSize`)
      example: an EcosystemSize of type Large has 240 Chunks
    - Every Ecosystem has the same size on generation

    ## Generation
    ### First -> Generate Biomes
    - First every Ecosystem is generated step by step
    - Every WorldMap has 6 Ecosystems
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

    ### Second ->
-}
{-
   - A Coordinate represents one hex in x and y axis system, it lives in a `Chunk`
-}


type alias Coordinate =
    { x : Int
    , y : Int
    }


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
    | Ground BaseMaterialClass FloraState GrowthRate (List MagicEffects) (List NaturalEffect) (List Entity)
    | Underground BaseMaterialClass FloraState GrowthRate (List MagicEffects) (List NaturalEffect) (List Entity)
    | DeepUnderground BaseMaterialClass (List MagicEffects) (List NaturalEffect) (List Entity)



{-
   A LayerConnector tells the order of layers
-}


type LayerConnector
    = LayerConnector ( Layer, LayerConnector )
    | LayerConnectionEnd


type Entity
    = Resource
    | PC MagicEffects
    | NPC MagicEffects


type Event
    = Environment


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



{-
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
   BiomeSpread means that the Biome does spread over multiple connected hexes consuming others (for creating larger forest or mountains)
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
    = MixedForest BiomeSpread -- (Every Forest has Biome Spread except BloodForest)
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
    | HillForest
    | EverGreenForest


type PlaneBiome
    = MixedPlane
    | DryPlane
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
    | DesertRock
    | DesertDarkRock
    | MagicRock
    | HillRock


type IceBiome
    = WhiteIce
    | BlackIce
    | BloodIce
    | DesertIce
    | RockIce
    | MagicIce
    | HillIce


type LavaBiome
    = FluidLava -- looks redish
    | MagicFluidLava -- maybe looks purple ? looses it's magic properties when it gets cold
    | ColdRockLava
    | BloodColdRockLava


type LakeBiome
    = WaterLake -- Has BiomeSpread
    | Oasis
    | BloodLake


type RiverBiome
    = WaterRiver -- Has BiomeSpread
    | BloodRiver


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
    | DeepMountain



{-
   ArtificialBiome replaces a chunks regular Biome via TerraForm spell, which means it can't be undone or treated as magical effect.
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



{-
   - WeatherEffect
   - EternalSnowStorm is deadly and permanent
-}


type WeatherEffects
    = Clear Temperature
    | Clouded Temperature
    | LightRain Temperature
    | MediumRain Temperature
    | HeavyRain Temperature
    | Monsoon Temperature
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
    | NormalTemp
    | Mild
    | Cold
    | VeryCold
    | IceCold


type NaturalEffect
    = PlantSaplings



{- EcoSystem
   - Contains information of possible Biomes, Weather System etc.
-}


type EcoSystemType
    = ModerateEcoSystemType


type EcoSystemSize
    = SmallEcoSystem
    | MediumEcoSystem
    | LargeEcoSystem
    | HugeEcoSystem



{-
   - seedList : A list of possible biomes, you can take rolls on it
   - Dice faces should equal the list length - 1, then pick by rolled index
   - share: how many of the given biome types in seedList will be in the ecosystem ?
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



{-
   A Moderate EcoSystem has the following rolling properties:
   (CHANGED!)
   RegularSeedList:
       1 Plane MixedPlane NormalTemp HighFertility HighHydration
       3 Forest MixedForest NormalTemp HighFertility MediumHydration
       1 Plane RiverPlane NormalTemp HighFertility HighHydration
       1 Forest RiverForest NormalTemp HighFertility MediumHydration
       1 Plane MixedPlane Warm MediumFertility LowHydration
       = 7

    SeldomSeedList:
        2 Forest DeepForest Mild MediumFertility LowHydration
        1 Forest DarkForest Cold MediumFertility MediumHydration
        1 Forest DarkForest Cold LowFertility MediumHydration
        2 Rock GreyRock NormalTemp LowFertility LowHydration
        1 Plane MixedPlane Warm HighFertility MediumHydration
        1 Forest MixedForest Warm HighFertility LowHydration
        1 Forest MixedForest Warm LowFertility Dehydrated
        2 Forest MixedForest NormalTemp HighFertility MediumHydration
        1 Lake WaterLake Cold LowFertility HighHydration
        1 Lake WaterLake NormalTemp HightFertility HighHydration
        = 13

    RareSeedList
        1 Plane DarkMixedPlane Mild MediumFertility LowHydration
        1 Plane RiverPlane Warm HighFertility HighHydration
        1 Forest RiverForest Warm HighFertility HighHydration
        1 Plane DryPlane Mild LowFertility Dehydrated
        1 Lake WaterLake Cold LowFertility HighHydration
        1 River WaterRiver Cold LowFertility HighHydration
        1 Rock DarkRock NormalTemp LowFertility Dehydrated
        1 Rock RiverRock NormalTemp LowFertility LowHydration
        2 Forest MixedForest NormalTemp HighFertility MediumHydration
        = 10

    UniqueSeedList
        1 Forest MagicForest Warm HighFertility HighHydration
        1 Forest LivingForest Warm NoFertility MediumHydration
        2 Rock DarkRock NormalTemp MediumFertility MediumHydration
        1 Plane MagicPlane Cold MediumFertility LowHydration
        1 Forest DreamForest VeryCold NoFertility LowHydration
        2 Forest DarkForest NormalTemp HighFertility MediumHydration
        2 Forest MixedForest Warm HighFertility HighHydration
        1 Plane DarkMixedPlane Mild HighFertility MediumHydration
        = 11

-}


getModerateEcoSystemBiomeSeedList : Occurrence -> List.Nonempty.Nonempty Biome
getModerateEcoSystemBiomeSeedList occurrence =
    case occurrence of
        RegularOccurrence ->
            -- RegularSeedList --
            List.Nonempty.Nonempty
                (Plane MixedPlane NormalTemp HighFertility HighHydration)
                [ Forest (MixedForest ThreeSpread) NormalTemp HighFertility MediumHydration
                , Plane (RiverPlane FourSpread) NormalTemp HighFertility HighHydration
                , Forest (MixedForest ThreeSpread) NormalTemp HighFertility MediumHydration
                , Forest (MixedForest ThreeSpread) Warm MediumFertility MediumHydration
                , Plane MixedPlane Warm MediumFertility LowHydration
                , Plane MixedPlane NormalTemp LowFertility LowHydration
                , Forest RiverForest NormalTemp HighFertility MediumHydration
                , Plane MixedPlane NormalTemp HighFertility HighHydration
                , Forest (DarkForest OneSpread) Cold MediumFertility LowHydration
                ]

        SeldomOccurrence ->
            -- SeldomSeedList --
            List.Nonempty.Nonempty
                (Forest DeepForest Mild MediumFertility LowHydration)
                [ Forest (DarkForest OneSpread) Cold MediumFertility MediumHydration
                , Forest DeepForest Mild MediumFertility LowHydration
                , Lake WaterLake Cold LowFertility HighHydration
                , Forest (DarkForest TwoSpread) Cold LowFertility MediumHydration
                , Forest (MixedForest FourSpread) NormalTemp HighFertility MediumHydration
                , Rock GreyRock NormalTemp LowFertility LowHydration
                , Plane MixedPlane Warm HighFertility MediumHydration
                , Rock GreyRock Cold LowFertility LowHydration
                , Forest (MixedForest FourSpread) NormalTemp HighFertility MediumHydration
                , Forest (MixedForest TwoSpread) Warm HighFertility HighHydration
                , Lake WaterLake NormalTemp HighFertility HighHydration
                , Rock GreyRock NormalTemp LowFertility LowHydration
                , Forest (MixedForest NoSpread) Warm LowFertility Dehydrated -- Rotten Forest
                ]

        RareOccurrence ->
            -- RareSeedList --
            List.Nonempty.Nonempty
                (Plane DarkMixedPlane Mild MediumFertility LowHydration)
                [ Plane (RiverPlane FiveSpread) Warm HighFertility HighHydration
                , Forest RiverForest Warm HighFertility HighHydration
                , Forest (MixedForest NoSpread) NormalTemp HighFertility MediumHydration
                , Plane DryPlane Mild LowFertility Dehydrated
                , Rock GreyRock Warm LowFertility HighHydration
                , Lake WaterLake Cold LowFertility HighHydration
                , River WaterRiver Cold LowFertility HighHydration
                , Forest (MixedForest OneSpread) NormalTemp HighFertility MediumHydration
                , Rock DarkRock NormalTemp LowFertility Dehydrated
                , Rock RiverRock NormalTemp LowFertility LowHydration
                , Forest (MixedForest OneSpread) Cold MediumFertility OverHydrated
                ]

        UniqueOccurrence ->
            List.Nonempty.Nonempty
                (Forest MagicForest Warm HighFertility HighHydration)
                [ Forest LivingForest Warm NoFertility MediumHydration
                , Forest (MixedForest NoSpread) NormalTemp HighFertility MediumHydration
                , Forest (DarkForest ThreeSpread) NormalTemp HighFertility MediumHydration
                , Rock DarkRock NormalTemp MediumFertility MediumHydration
                , Plane MagicPlane Cold MediumFertility LowHydration
                , Forest (MixedForest NoSpread) Warm HighFertility MediumHydration
                , Forest DreamForest VeryCold NoFertility LowHydration
                , Rock DarkRock NormalTemp MediumFertility MediumHydration
                , Forest (DarkForest OneSpread) NormalTemp HighFertility HighHydration
                , Plane DarkMixedPlane Mild HighFertility MediumHydration
                , Forest (MixedForest NoSpread) Warm LowFertility Dehydrated -- Rotten Forest - Maybe Cursed ?
                ]



-- Resources and Build Material --


type BuildMaterialClass
    = Wood
    | Stone
    | Gem
    | Bone
    | Metal
    | Leather



-- type Resources
-- type WoodResource
{-
   - Magic Energie drains from the Magic caster like fluid. It can be cast without materials but it weakens the caster, like dehydrating
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
    = ThinkAboutIt


type Spell
    = IncreaseHydration
    | IncreaseFertility
    | TerraForm


type SpellResources
    = Feather


type MagicShool
    = Terra -- Magic Shool which does focus on altering the natural enviroment



-- getSpellCost : Spell a -> Complexity -> { spellResources : List SpellResources, magicEnergy : MagicEnergy }
-- Characters --


type CharacterClass
    = Fighter
    | Hunter
    | Gatherer
    | Craftsmen
    | Healer
