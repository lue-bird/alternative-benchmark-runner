module Humanize exposing (int, percent)


int : Int -> String
int =
    String.fromInt
        >> String.toList
        >> List.reverse
        >> groupsOf 3
        >> List.reverse
        >> List.map (List.reverse >> String.fromList)
        >> String.join ","


percent : Float -> String
percent =
    (*) 10000
        >> round
        >> (\a -> toFloat a / 100)
        >> String.fromFloat
        >> (\a -> a ++ "%")



-- utils


groupsOf : Int -> List a -> List (List a)
groupsOf groupSize xs =
    case List.take groupSize xs of
        [] ->
            []

        firstGroup ->
            firstGroup
                :: (xs
                        |> List.drop groupSize
                        |> groupsOf groupSize
                   )
