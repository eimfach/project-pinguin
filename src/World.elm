module World exposing (Occurrence(..))

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
    ### First
    - First every Ecosystem is generated step by step
    - Every Ecosystem has a type `type EcosystemType`
    - Now for every Ecosystem multiple Lists of random Biomes is created
    - Every List represents Biomes of every Occurence (`type Occurence`)
    - A function receiving `type Occurrence` and the `type EcosystemType`
      returns a List of possible Biomes for every Occurrence (called BiomeSeedList)
    - The Share tells how large the generated list will be
    - Every Occurrence reflects the Share in EcoSystemSize
    - Occurrences:
     type Occurrence
         = RegularOccurrence (60%)
         | SeldomOccurrence (20%)
         | RareOccurrence (15%  of EcoSystemSize)
         | UniqueOccurrence (5% of EcoSystemSize)
   - The generated Biome Lists are then merged into one List and this List is mixed randomly

    - This World Module exposes a function which is receiving `type Occurrence`, `type EcosystemType` and `type EcoSystemSize`
      and returns a List of possible Biomes for every Occurrence, and the Share as an Int.

    - The random generation of Biomes works as following
    - For every generated List a dice is rolled where face count equals the length of the BiomeSeedList
    - The dice result then will be used as an index to get a Biome from the BiomeSeedList. This Biome will be added to the generated List.
-}
{-
   - A Coordinate represents one hex
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
    | Ambitous
    | Hard
    | VeryHard
    | Godlike



{-
   - [x] A Chunk is a single ingame field
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
   - A Layer has enviromental properties like material class (stone, sand, rock...)
     or possible resources, flora class and flora states like water supply which leads to grow rate
   - A Layer can have multiple magical effects on it (like magic hydration or alteration of flora class)
   - Layers can be permantely altered after some time applying magic to it:
    -> like changing the weather or, with mighty magic, even the BaseMaterialClass or biome, for example:
        -> You can create lakes or rivers, enchant forests, create forest illusions, soften rock for easier mining,
           curse forests or improve fertility/hydration or just cast rain, change the temperature,
           create forests in deserts (but desertification)
        -> Note: You can curse forests around you, to build a defensive wall, and they won't change appearance.
           However, cursed trees can't be chopped until the curse is removed. There can be a lot of ways to curse a forest.
        -> Or you can apply other magical effects onto a layer, like a magical shield around your village where nobody can get in or out for some time.
-}


type Layer
    = Atmosphere (List MagicEffects) WeatherEffect
    | Ground BaseMaterialClass FloraState (List MagicEffects) (List Entity)
    | Underground BaseMaterialClass FloraState (List MagicEffects) (List Entity)
    | DeepUnderground BaseMaterialClass (List MagicEffects) (List Entity)



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
    = Enviroment


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



{-
   - A Biome has a name which reflects something like the weather, monsters, enviroment or resources
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
    | Artificial ArtificialBiome Temperature Fertility Hydration


type ForestBiome
    = MixedForest
    | DeepForest
    | DarkForest
    | DeepDarkForest
    | RiverForest
    | LivingForest
    | BloodForest
    | DreamForest
    | RainForest
    | IceForest
    | MagicForest


type PlaneBiome
    = MixedPlane
    | DryPlane
    | DarkMixedPlane
    | RiverPlane
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


type IceBiome
    = WhiteIce
    | BlackIce
    | BloodIce
    | DesertIce
    | RockIce
    | MagicIce


type LavaBiome
    = FluidLava -- looks redish
    | MagicFluidLava -- maybe looks purple ? looses it's magic properties when it gets cold
    | ColdRockLava
    | BloodColdRockLava


type LakeBiome
    = WaterLake
    | Oasis
    | BloodLake


type RiverBiome
    = WaterRiver
    | BloodRiver


type OceanBiome
    = SaltyWaterOcean


type DesertBiome
    = SandDesert
    | DarkSandDesert
    | BloodDesert
    | LostDesert



{-
   ArtificialBiome replaces a chunks regular Biome via TerraForm spell, which means it can't be undone or treated as magical effect.
   Only Weather can change the Biome after it. But you can control Weather
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



{-
   - WeatherEffect
   - EternalSnowStorm is deadly and permanent
-}


type WeatherEffect
    = Clear Temperature
    | Clouded Temperature
    | LightRain Temperature
    | MediumRain Temperature
    | HeavyRain Temperature
    | Monsun Temperature
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
    = PlantSapling



{- EcoSystem
   - Contains information of possible Biomes, Weather System etc.
-}


type EcoSystemType
    = ModerateEcoSystem


type EcoSystemSize
    = Small
    | Medium
    | Large
    | Huge


translateEcoSystemSize : EcoSystemSize -> Int
translateEcoSystemSize ecoSystemSize =
    case ecoSystemSize of
        Small ->
            60

        Medium ->
            120

        Large ->
            240

        Huge ->
            480


getEcoSystemBiomeSeedList : EcoSystemType -> Occurrence -> List.Nonempty.Nonempty Biome
getEcoSystemBiomeSeedList ecoSystemType occurrence =
    case ecoSystemType of
        ModerateEcoSystem ->
            getModerateEcoystemBiomeSeedList occurrence



{-
   A Moderate EcoSystem has the following rolling properties:
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
        2 Forest MixedForest NormalTemp HighFertility MediumHydration
        1 Plane DarkMixedPlane Mild HighFertility MediumHydration
        = 11

-}


getModerateEcoystemBiomeSeedList : Occurrence -> List.Nonempty.Nonempty Biome
getModerateEcoystemBiomeSeedList occurrence =
    case occurrence of
        RegularOccurrence ->
            -- RegularSeedList --
            createNonEmptyList
                (Plane MixedPlane NormalTemp HighFertility HighHydration)
                [ Forest MixedForest NormalTemp HighFertility MediumHydration
                , Plane RiverPlane NormalTemp HighFertility HighHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Plane MixedPlane Warm MediumFertility LowHydration
                , Forest RiverForest NormalTemp HighFertility MediumHydration
                ]

        SeldomOccurrence ->
            -- SeldomSeedList --
            createNonEmptyList
                (Forest DeepForest Mild MediumFertility LowHydration)
                [ Forest DarkForest Cold MediumFertility MediumHydration
                , Forest DeepForest Mild MediumFertility LowHydration
                , Lake WaterLake Cold LowFertility HighHydration
                , Forest DarkForest Cold LowFertility MediumHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Rock GreyRock NormalTemp LowFertility LowHydration
                , Plane MixedPlane Warm HighFertility MediumHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Forest MixedForest Warm HighFertility HighHydration
                , Lake WaterLake NormalTemp HighFertility HighHydration
                , Rock GreyRock NormalTemp LowFertility LowHydration
                , Forest MixedForest Warm LowFertility Dehydrated
                ]

        RareOccurrence ->
            -- RareSeedList --
            createNonEmptyList
                (Plane DarkMixedPlane Mild MediumFertility LowHydration)
                [ Plane RiverPlane Warm HighFertility HighHydration
                , Forest RiverForest Warm HighFertility HighHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Plane DryPlane Mild LowFertility Dehydrated
                , Lake WaterLake Cold LowFertility HighHydration
                , River WaterRiver Cold LowFertility HighHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Rock DarkRock NormalTemp LowFertility Dehydrated
                , Rock RiverRock NormalTemp LowFertility LowHydration
                ]

        UniqueOccurrence ->
            createNonEmptyList
                (Forest MagicForest Warm HighFertility HighHydration)
                [ Forest LivingForest Warm NoFertility MediumHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Forest DarkForest NormalTemp HighFertility MediumHydration
                , Rock DarkRock NormalTemp MediumFertility MediumHydration
                , Plane MagicPlane Cold MediumFertility LowHydration
                , Forest MixedForest NormalTemp HighFertility MediumHydration
                , Forest DreamForest VeryCold NoFertility LowHydration
                , Rock DarkRock NormalTemp MediumFertility MediumHydration
                , Forest DarkForest NormalTemp HighFertility MediumHydration
                , Plane DarkMixedPlane Mild HighFertility MediumHydration
                ]


createNonEmptyList : a -> List a -> List.Nonempty.Nonempty a
createNonEmptyList anElement aList =
    List.foldl (List.Nonempty.fromElement >> List.Nonempty.append) (List.Nonempty.fromElement anElement) aList



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
    | Weather WeatherEffect
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
