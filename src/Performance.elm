module Performance exposing (equality, member)


member : Int -> List Int -> Bool
member some inList =
    List.any (equality some) inList


equality : Int -> Int -> Bool
equality a b =
    (a - b) == 0
