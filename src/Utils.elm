module Utils exposing (insert,move, split, find, findIndex,positionIsEqual, convertData, findInsertionIndex, emptyHtml)

import AppData exposing (AppData, AppDataNext, DropTargetPosition(..), SectionData(..), SectionDataNext(..))
import Dict
import Html
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

findInsertionIndex itemOrderList targetItemId dropTargetPosition =
        LE.findIndex (\id -> targetItemId == id) itemOrderList
            |> Maybe.map
                (\i ->
                    case dropTargetPosition of
                        Before ->
                            i

                        After ->
                            i + 1
                )

positionIsEqual x y =
    case ( x, y ) of
        ( Before, Before ) ->
            True

        ( After, After ) ->
            True
        _ ->
            False


convertData : AppData -> AppDataNext
convertData data =
    let
        newAcc =
            { sections = Dict.empty
            , orderLists = Dict.singleton "sections" []
            , tags = Dict.empty
            , items = Dict.empty
            }
    in
    List.foldr
        (\section acc ->
            case section of
                GalleryWithTagsSectionType sectionData ->
                    let
                        id =
                            sectionData.sectionId

                        itemOrderId =
                            sectionData.sectionId ++ "_items"

                        tagOrderId =
                            sectionData.sectionId ++ "_tags"

                        sectionItems =
                            List.map (\{ itemId, fileName, urlString } -> ( itemId, AppData.ItemDataNext itemId fileName urlString [ itemOrderId ] )) sectionData.items
                                |> Dict.fromList

                        sectionItemOrder =
                            [ ( itemOrderId, List.map (\{ itemId } -> itemId) sectionData.items ) ]
                                |> Dict.fromList

                        sectionTags =
                            List.map (\{ tagId, label, items } -> ( tagId, AppData.TagDataNext tagId label (itemOrderId ++ "_" ++ tagId) [ tagOrderId ] )) sectionData.tags
                                |> Dict.fromList

                        sectionTagOrder =
                            [ ( tagOrderId, List.map (\{ tagId } -> tagId) sectionData.tags ) ]
                                |> Dict.fromList

                        tagItemOrders =
                            List.map (\tag -> ( itemOrderId ++ "_" ++ tag.tagId, List.map (\{ itemId } -> itemId) tag.items )) sectionData.tags
                                |> Dict.fromList

                        itemsWithTagUsage =
                            List.foldl
                                (\tag acc1 ->
                                    List.foldl
                                        (\{ itemId } acc2 ->
                                            Dict.update itemId
                                                (Maybe.map (\item -> { item | usedIn = (itemOrderId ++ "_" ++ tag.tagId) :: item.usedIn }))
                                                acc2
                                        )
                                        acc1
                                        tag.items
                                )
                                sectionItems
                                sectionData.tags

                        orderLists =
                            Dict.update "sections"
                                (\maybeList ->
                                    case maybeList of
                                        Just sectionOrder ->
                                            Just (sectionData.sectionId :: sectionOrder)

                                        Nothing ->
                                            Nothing
                                )
                                acc.orderLists
                                |> Dict.union sectionOrderLists

                        sectionOrderLists =
                            Dict.union sectionItemOrder tagItemOrders
                                |> Dict.union sectionTagOrder

                        sectionNext =
                            { sectionId = sectionData.sectionId
                            , label = sectionData.label
                            , tagOrderId = tagOrderId
                            , itemOrderId = itemOrderId
                            , usedIn = [ "sections" ]
                            }
                                |> GalleryWithTagsSectionNext
                    in
                    { acc
                        | items = Dict.union itemsWithTagUsage acc.items
                        , tags = Dict.union sectionTags acc.tags
                        , orderLists = orderLists
                        , sections = Dict.insert sectionData.sectionId sectionNext acc.sections
                    }

                _ ->
                    acc
        )
        newAcc
        data

emptyHtml =
    Html.text ""