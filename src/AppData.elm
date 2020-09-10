module AppData exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import Types exposing (AppDataNext, ItemData, OrderListId, SaveImageResponse, SectionData(..), TagData)



--JSON --


saveImageDecoder : JD.Decoder SaveImageResponse
saveImageDecoder =
    JD.map2 SaveImageResponse
        (JD.field "itemId" JD.string)
        (JD.field "fileName" JD.string)


appDataDecoder : JD.Decoder (List SectionData)
appDataDecoder =
    JD.field "sections" (JD.list sectionDataDecoder)


itemDataDecoder : JD.Decoder ItemData
itemDataDecoder =
    JD.map3 ItemData
        (JD.field "itemId" JD.string)
        (JD.field "fileName" JD.string)
        (JD.field "urlString" JD.string)


tagDataDecoder : JD.Decoder TagData
tagDataDecoder =
    JD.map3 TagData
        (JD.field "label" JD.string)
        (JD.field "tagId" JD.string)
        (JD.field "items" (JD.list itemDataDecoder))


itemsDecoder =
    JD.list itemDataDecoder


sectionDataDecoder : JD.Decoder SectionData
sectionDataDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\sectionType ->
                case sectionType of
                    "galleryWithTags" ->
                        JD.map4 Types.GalleryWithTagsSectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            (JD.field "items" itemsDecoder)
                            (JD.field "tags" (JD.list tagDataDecoder))
                            |> JD.map GalleryWithTagsSectionType

                    "gallery" ->
                        JD.map2 Types.GallerySectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            |> JD.map GallerySectionType

                    "info" ->
                        JD.map4 Types.InfoSectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            (JD.field "text" JD.string)
                            (JD.field "imageId" JD.string)
                            |> JD.map InfoSectionType

                    _ ->
                        JD.fail "no luck today"
            )



----- Encoders ---------------


encodeAppData appData =
    JE.list sectionEncoder appData


sectionEncoder section =
    case section of
        GalleryWithTagsSectionType data ->
            JE.object
                [ ( "label", JE.string data.label )
                , ( "sectionId", JE.string data.sectionId )
                , ( "items", JE.list itemEncoder data.items )
                , ( "tags", JE.list tagEncoder data.tags )
                ]

        _ ->
            JE.object []



--todo


tagEncoder tag =
    JE.object
        [ ( "label", JE.string tag.label )
        , ( "tagId", JE.string tag.tagId )
        , ( "items", JE.list itemEncoder tag.items )
        ]


itemEncoder item =
    JE.object
        [ ( "itemId", JE.string item.itemId )
        , ( "fileName", JE.string item.fileName )
        , ( "urlString", JE.string item.urlString )
        ]



-- updates


setItemOrderList : AppDataNext -> Maybe (List String) -> Maybe OrderListId -> Maybe AppDataNext
setItemOrderList dataNext maybeNewItemOrderList maybeItemOrderListId =
    Maybe.map2
        (\itemOrderListId newItemOrderList ->
            Dict.update itemOrderListId (\_ -> Just newItemOrderList) dataNext.orderLists
        )
        maybeItemOrderListId
        maybeNewItemOrderList
        |> Maybe.map
            (\orderLists ->
                { dataNext | orderLists = orderLists }
            )


updateItemList dataNext tagId itemId fn =
    let
        maybeItemOrderListId =
            Dict.get tagId dataNext.tags
                |> Maybe.map (\tag -> tag.itemOrderId)

        maybeItemOrderList =
            maybeItemOrderListId
                |> Maybe.andThen (\itemOrderId -> Dict.get itemOrderId dataNext.orderLists)

        maybeItemIndex =
            maybeItemOrderList
                |> Maybe.andThen (\itemOrderList -> LE.findIndex (\i -> i == itemId) itemOrderList)

        maybeNewItemOrderList =
            Maybe.map2
                fn
                maybeItemIndex
                maybeItemOrderList
    in
    setItemOrderList dataNext maybeNewItemOrderList maybeItemOrderListId
