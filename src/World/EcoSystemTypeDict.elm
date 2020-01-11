module World.EcoSystemTypeDict exposing (EcoSystemTypeDict, construct, empty, get, insert, toList)

import List.Extra exposing (allDifferent)
import List.Nonempty
import World


type EcoSystemTypeDict
    = EcoSystemTypeDict (List ( World.EcoSystemType, List.Nonempty.Nonempty World.Biome ))


parseEcoSystemType : World.EcoSystemType -> Int
parseEcoSystemType ecoSystemType =
    case ecoSystemType of
        World.ModerateEcoSystemType ->
            1

        World.MoonEcoSystemType ->
            2


allDifferentEcoSystemTypes : List ( World.EcoSystemType, List.Nonempty.Nonempty World.Biome ) -> Bool
allDifferentEcoSystemTypes unitList =
    unitList
        |> List.map (\( ecoSystem, _ ) -> ecoSystem)
        |> List.map parseEcoSystemType
        |> allDifferent


empty : EcoSystemTypeDict
empty =
    EcoSystemTypeDict []



{-
   The smart constructor for this module. When given duplicate keys, `Nothing` will be returned.
-}


construct : List ( World.EcoSystemType, List.Nonempty.Nonempty World.Biome ) -> Maybe EcoSystemTypeDict
construct unitList =
    if allDifferentEcoSystemTypes unitList == False then
        Nothing

    else
        Just (EcoSystemTypeDict unitList)


toList : EcoSystemTypeDict -> List ( World.EcoSystemType, List.Nonempty.Nonempty World.Biome )
toList dict =
    case dict of
        EcoSystemTypeDict unitList ->
            unitList


insert : World.EcoSystemType -> List.Nonempty.Nonempty World.Biome -> EcoSystemTypeDict -> EcoSystemTypeDict
insert newEcoSystemType biomeList dict =
    case dict of
        EcoSystemTypeDict unitList ->
            unitList
                |> List.filter (\( ecoType, _ ) -> ecoType /= newEcoSystemType)
                |> List.append [ ( newEcoSystemType, biomeList ) ]
                |> EcoSystemTypeDict


get : World.EcoSystemType -> EcoSystemTypeDict -> Maybe (List.Nonempty.Nonempty World.Biome)
get whichEcoSystemType dict =
    case dict of
        EcoSystemTypeDict unitList ->
            unitList
                |> List.Extra.find (\( ecoType, _ ) -> ecoType == whichEcoSystemType)
                |> Maybe.andThen (\( _, biomeList ) -> Just biomeList)
