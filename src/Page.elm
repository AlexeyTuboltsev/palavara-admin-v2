module Page exposing (..)

import AppData exposing (AppDataNext, DropTargetPosition(..), GalleryWithTagsSectionDataNext, ItemDataNext, ItemId, OrderListId, SectionDataNext(..), SectionId, TagData, TagDataNext, TagId)
import Dict exposing (Dict)
import Maybe.Extra as ME


type View
    = Initial
    | Section SectionId


type Modal
    = ModalClosed
    | ConfirmDeleteItem TagId ItemId


type alias ItemEditorData =
    { itemId : ItemId
    , fileName : Maybe String
    , src: Maybe String
    , urlString : Maybe String
    , usedIn : List OrderListId
    }


type ItemEditor
    = ItemEditorClosed
    | ItemEditorOpen ItemEditorData
    | ItemEditorLoading ItemEditorData


type alias UIData =
    { view : View
    , imageUrl : String
    , dnd : Maybe ( TagId, ItemId )
    , dragOver : Maybe ( TagId, ItemId, DropTargetPosition )
    , modal : Modal
    , itemEditor : ItemEditor
    }


type Page
    = SectionPage SectionPageData
    | InitialPage InitialPageData


type alias InitialPageData =
    { sections : List ( SectionId, String )
    }


type alias SectionPageData =
    { sections : List ( SectionId, String )
    , activeSectionId : SectionId
    , tags : List TagViewData
    , items : List ItemViewData
    , dnd : Maybe ( TagId, ItemId )
    , dragOver : Maybe ( TagId, ItemId, DropTargetPosition )
    }


type alias ItemViewData =
    { itemId : ItemId
    , fileName : String
    , urlString : String
    , src : String
    , isDnD : Bool
    , dndOnOver : Maybe DropTargetPosition
    }


type alias TagViewData =
    { label : String
    , tagId : TagId
    , items : List ItemViewData
    }


generateItemViewData : Dict ItemId ItemDataNext -> UIData -> ItemId -> Maybe ItemViewData
generateItemViewData items uiData itemId =
    Dict.get itemId items
        |> Maybe.map
            (\{ fileName, urlString } ->
                let
                    maybeDragOver =
                        case uiData.dragOver of
                            Just ( _, id, position ) ->
                                case id == itemId of
                                    True ->
                                        Just position

                                    False ->
                                        Nothing

                            Nothing ->
                                Nothing

                    isDnD =
                        case uiData.dnd of
                            Just ( _, id ) ->
                                id == itemId

                            Nothing ->
                                False
                in
                { itemId = itemId
                , fileName = fileName
                , urlString = urlString
                , src = uiData.imageUrl ++ fileName
                , isDnD = isDnD
                , dndOnOver = maybeDragOver
                }
            )


generateItemListViewData : AppDataNext -> UIData -> String -> Maybe (List ItemViewData)
generateItemListViewData appDataNext uiData itemOrderId =
    Dict.get itemOrderId appDataNext.orderLists
        |> Maybe.map (\itemOrderList -> List.map (generateItemViewData appDataNext.items uiData) itemOrderList)
        |> Maybe.map ME.values


getTagData : AppDataNext -> String -> Maybe (List TagDataNext)
getTagData appDataNext tagOrderId =
    Dict.get tagOrderId appDataNext.orderLists
        |> Maybe.map
            (\tagOrderList -> List.map (\tagId -> Dict.get tagId appDataNext.tags) tagOrderList)
        |> Maybe.map ME.values


generateTagListViewData : AppDataNext -> UIData -> String -> Maybe (List TagViewData)
generateTagListViewData appDataNext uiData tagOrderId =
    getTagData appDataNext tagOrderId
        |> Maybe.map
            (\tags ->
                List.map (generateTagViewData appDataNext uiData) tags
            )


generateTagViewData : AppDataNext -> UIData -> TagDataNext -> TagViewData
generateTagViewData appDataNext uiData tagData =
    let
        items =
            generateItemListViewData appDataNext uiData tagData.itemOrderId
                |> Maybe.withDefault []
    in
    { tagId = tagData.tagId, label = tagData.label, items = items }


generateMenuViewData : AppDataNext -> UIData -> (SectionDataNext -> a) -> Maybe (List a)
generateMenuViewData appDataNext uiData convertFn =
    Dict.get "sections" appDataNext.orderLists
        |> Maybe.map
            (\sectionIds ->
                List.map (\sectionId -> Dict.get sectionId appDataNext.sections) sectionIds
                    |> ME.values
            )
        |> Maybe.map
            (\sections ->
                List.map (\section -> convertFn section) sections
            )


generateGalleryWithTagsSectionViewData : AppDataNext -> UIData -> GalleryWithTagsSectionDataNext -> Maybe Page
generateGalleryWithTagsSectionViewData appDataNext uiData sectionData =
    let
        maybeSectionData =
            generateMenuViewData appDataNext uiData sectionDataToSectionViewData

        maybeItems =
            generateItemListViewData appDataNext uiData sectionData.itemOrderId

        maybeTags =
            generateTagListViewData appDataNext uiData sectionData.tagOrderId
    in
    Maybe.map3
        (\sections tags items ->
            { activeSectionId = sectionData.sectionId, sections = sections, tags = tags, items = items, dnd = Nothing, dragOver = Nothing }
        )
        maybeSectionData
        maybeTags
        maybeItems
        |> Maybe.map (\s -> SectionPage s)


generatePageData : AppDataNext -> UIData -> Maybe Page
generatePageData appDataNext uiData =
    case uiData.view of
        Section sectionId ->
            Dict.get sectionId appDataNext.sections
                |> Maybe.andThen
                    (\section ->
                        case section of
                            GalleryWithTagsSectionNext sectionData ->
                                generateGalleryWithTagsSectionViewData appDataNext uiData sectionData

                            _ ->
                                Nothing
                    )

        _ ->
            Nothing


sectionDataToSectionViewData section =
    case section of
        GalleryWithTagsSectionNext sectionData ->
            ( sectionData.sectionId, sectionData.label )

        GallerySectionNext sectionData ->
            ( sectionData.sectionId, sectionData.label )

        InfoSectionNext sectionData ->
            ( sectionData.sectionId, sectionData.label )


setActiveSection uiData sectionId =
    { uiData | view = Section sectionId }


startDnD uiData tagId itemId =
    { uiData | dnd = Just ( tagId, itemId ) }


stopDnD uiData =
    { uiData | dnd = Nothing }


addDragOver uiData tagId itemId dropTargetPosition =
    { uiData | dragOver = Just ( tagId, itemId, dropTargetPosition ) }


removeDragOver uiData =
    { uiData | dragOver = Nothing }


debugLog s x =
    let
        _ =
            Debug.log s x
    in
    x
