module Colors exposing (..)

import Color
import Color.Convert exposing (colorToHex)
import Color.Manipulate exposing (darken, lighten, rotateHue, saturate, scaleHsl)


{-| #326c95
-}
basicDeepOceanBackgroundColor =
    Color.rgb255 50 108 149


basicDeepOceanBackgroundColorAsHex =
    colorToHex basicDeepOceanBackgroundColor


deepOceanObjectColor =
    Color.rgb255 0 151 178


deepOceanObjectColorAsHex =
    colorToHex deepOceanObjectColor


{-| #428742
-}
basicLandMassBackgroundColor =
    Color.rgb255 66 135 66


moonLandMassBackgroundColor =
    Color.rgb255 109 0 163



-- #428742


basicLakeBackgroundColor =
    Color.rgb255 31 180 255


moderateTreeStemBaseColor =
    Color.rgb255 114 90 69


moderateTreeBaseColor =
    Color.rgb255 40 85 40


moderateTreeLeavesColor =
    Color.rgb255 53 110 53


moderateMagicTreeBaseColor =
    moonMagicTreeBaseColor


moderateMagicTreeLeavesColor =
    moonMagicTreeLeavesColor


moonTreeBaseColor =
    Color.rgb255 36 5 117


moonTreeLeavesColor =
    lighten 0.1 moonTreeBaseColor


moonMagicTreeBaseColor =
    rotateHue 25.0 moonTreeBaseColor


moonMagicTreeLeavesColor =
    lighten 0.1 moonMagicTreeBaseColor


invalidColor =
    Color.red


baseLakeColor =
    Color.rgb255 31 180 255


wavesLakeColor =
    Color.rgb255 33 222 255


mixedPlaneObjectsColor =
    Color.rgb255 66 135 100
