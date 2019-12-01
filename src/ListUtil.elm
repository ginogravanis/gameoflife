module ListUtil exposing (subdivideList)


type alias Chunks a =
    List (List a)


subdivideList : Int -> List a -> Chunks a
subdivideList chunkSize list =
    if chunkSize > 0 then
        subdivideListHelper chunkSize list []

    else
        subdivideListHelper 1 list []


subdivideListHelper : Int -> List a -> Chunks a -> Chunks a
subdivideListHelper size list acc =
    if List.isEmpty list then
        List.reverse acc

    else
        subdivideListHelper
            size
            (List.drop size list)
            (List.take size list :: acc)
