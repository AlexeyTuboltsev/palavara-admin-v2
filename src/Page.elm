module Page exposing (..)

import AppData exposing (AppDataNext, DropTargetPosition, GalleryWithTagsSectionDataNext, ItemDataNext, ItemId, SectionDataNext(..), SectionId, TagData, TagDataNext, TagId)
import Dict exposing (Dict)
import Maybe.Extra as ME

type View = Initial | Section SectionId

type alias UIData =
    { view: View

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
    , isLoading : Bool
    }


type alias TagViewData =
    { label : String
    , tagId : TagId
    , items : List ItemViewData
    }


generateItemViewData : Dict ItemId ItemDataNext -> ItemId -> Maybe ItemViewData
generateItemViewData items itemId =
    Dict.get itemId items
        |> Maybe.map
            (\{ fileName, urlString } ->
                { itemId = itemId
                , fileName = fileName
                , urlString = urlString
                , src = urlString
                , isLoading = False
                }
            )


generateItemListViewData : AppDataNext -> String -> Maybe (List ItemViewData)
generateItemListViewData appDataNext itemOrderId =
    Dict.get itemOrderId appDataNext.orderLists
        |> Maybe.map (\itemOrderList -> List.map (generateItemViewData appDataNext.items) itemOrderList)
        |> Maybe.map ME.values


getTagData : AppDataNext -> String -> Maybe (List TagDataNext)
getTagData appDataNext tagOrderId =
    Dict.get tagOrderId appDataNext.orderLists
        |> Maybe.map
            (\tagOrderList -> List.map (\tagId -> Dict.get tagId appDataNext.tags) tagOrderList)
        |> Maybe.map ME.values


generateTagListViewData : AppDataNext -> String -> Maybe (List TagViewData)
generateTagListViewData appDataNext tagOrderId =
    getTagData appDataNext tagOrderId
        |> Maybe.map
            (\tags ->
                List.map (generateTagViewData appDataNext) tags
            )


generateTagViewData : AppDataNext -> TagDataNext -> TagViewData
generateTagViewData appDataNext tagData =
    let
        items =
            generateItemListViewData appDataNext tagData.itemOrderId
                |> Maybe.withDefault []
    in
    { tagId = tagData.tagId, label = tagData.label, items = items }


generateMenuViewData : AppDataNext -> (SectionDataNext -> a) -> Maybe (List a)
generateMenuViewData appDataNext convertFn =
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


generateGalleryWithTagsSectionViewData : AppDataNext -> GalleryWithTagsSectionDataNext -> Maybe Page
generateGalleryWithTagsSectionViewData appDataNext sectionData =
    let
        maybeSectionData =
            generateMenuViewData appDataNext sectionDataToSectionViewData

        maybeItems =
            generateItemListViewData appDataNext sectionData.itemOrderId

        maybeTags =
            generateTagListViewData appDataNext sectionData.tagOrderId
    in
    Maybe.map3
        (\sections tags items ->
            { activeSectionId = sectionData.sectionId, sections = sections, tags = tags, items = items, dnd = Nothing, dragOver = Nothing }
        )
        maybeSectionData
        maybeTags
        maybeItems
        |> Maybe.map (\s -> SectionPage s)


generateSectionViewData : AppDataNext -> UIData -> Maybe Page
generateSectionViewData appDataNext uiData =
    case uiData.view of
        Section sectionId ->
            Dict.get sectionId appDataNext.sections
                |> Maybe.andThen
                    (\section ->
                        case section of
                            GalleryWithTagsSectionNext sectionData ->
                                generateGalleryWithTagsSectionViewData appDataNext sectionData

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
