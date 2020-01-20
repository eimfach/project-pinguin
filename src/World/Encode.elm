module World.Encode exposing (..)

import World


hydrationToString : World.Hydration -> String
hydrationToString hydration =
    case hydration of
        World.Dehydrated ->
            "Dehydrated"

        World.LowHydration ->
            "LowHydration"

        World.MediumHydration ->
            "MediumHydration"

        World.HighHydration ->
            "HighHydration"

        World.PerfectHydration ->
            "PerfectHydration"

        World.OverHydrated ->
            "OverHydrated"


fertilityToString : World.Fertility -> String
fertilityToString fertility =
    case fertility of
        World.NoFertility ->
            "NoFertility"

        World.LowFertility ->
            "LowFertility"

        World.MediumFertility ->
            "MediumFertility"

        World.HighFertility ->
            "HighFertility"

        World.PerfectFertility ->
            "PerfectFertility"


temperatureToString : World.Temperature -> String
temperatureToString temperature =
    case temperature of
        World.MeltingHot ->
            "MeltingHot"

        World.VeryHot ->
            "VeryHot"

        World.Hot ->
            "Hot"

        World.Warm ->
            "Warm"

        World.AverageTemp ->
            "AverageTemp"

        World.Mild ->
            "Mild"

        World.Cold ->
            "Cold"

        World.VeryCold ->
            "VeryCold"

        World.IceCold ->
            "IceCold"
