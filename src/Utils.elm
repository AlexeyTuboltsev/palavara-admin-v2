module Utils exposing (insert,move, split, find, findIndex,positionIsEqual)

import AppData exposing (DropTargetPosition(..))
import List.Extra as LE

insert targetIndex list item =
    case targetIndex of
        0 ->
            item :: list

        _ ->
            let
                ( a, b ) =
                    LE.splitAt targetIndex list
            in
            List.concat [ a, [ item ], b ]

move : Int -> Int -> Int -> List a -> List a
move originalPositionIndex originalLength targetPositionIndex list =
    if
        (originalPositionIndex <= targetPositionIndex)
            && ((originalPositionIndex + originalLength) >= targetPositionIndex)
    then
        list

    else if (originalPositionIndex + originalLength) > List.length list || originalPositionIndex < 0 then
        list

    else if targetPositionIndex > List.length list || targetPositionIndex < 0 then
        list

    else
        split [ originalPositionIndex, originalPositionIndex + originalLength, targetPositionIndex ] list
            |> (\acc ->
                    case acc of
                        [ a, b, c, d ] ->
                            List.concat [ a, c, b, d ]

                        _ ->
                            []
               )


split : List Int -> List a -> List (List a)
split points list =
    points
        |> LE.unique
        |> List.sort
        |> List.foldr
            (\point ( rest, acc ) ->
                LE.splitAt point rest
                    |> (\( nextRest, x ) -> ( nextRest, x :: acc ))
            )
            ( list, [] )
        |> (\( last, acc ) -> last :: acc)

findIndex predicate list errMessage =
    LE.findIndex predicate list
        |> Result.fromMaybe errMessage

find predicate list errMessage =
    LE.find predicate list
        |> Result.fromMaybe errMessage

positionIsEqual x y =
    case ( x, y ) of
        ( Before, Before ) ->
            True

        ( After, After ) ->
            True

        _ ->
            False