module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation exposing (Key, load)
import Dict exposing (..)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (alt, class, href, id, property, src, style)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Events.Extra.Drag exposing (DropEffect(..), onDropTarget, onSourceDrag)
import Html.Keyed exposing (node)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Http exposing (expectJson, get)
import Icons
import List.Extra as LE exposing (find, findIndex, getAt, removeAt, setAt, setIf, splitAt, unique, updateAt)
import Maybe.Extra as ME
import Message exposing (Msg(..))
import Page exposing (ItemViewData, Page(..), SectionPageData, TagViewData, UIData, View(..), addDragOver, generateMenuViewData, generatePageData, removeDragOver, sectionDataToSectionViewData, setActiveSection, startDnD, stopDnD)
import Result exposing (Result)
import Task
import Url exposing (Url)
import Utils exposing (convertData, find, findIndex, insert, move, positionIsEqual)


type alias Flags =
    { apiBaseUrl : String
    , apiProtocol : String
    , apiPort : String
    , dataPath : String
    , imagePath : String
    }


type alias ReadyModelData =
    { key : Navigation.Key
    , apiUrl : String
    , page : Page
    , data : AppData
    , tempData : Maybe AppData
    , dataNext : AppDataNext
    , dnd : Maybe ( TagId, ItemId )
    , uiData : UIData
    }


type alias InitModelData =
    { url : Url
    , imageUrl : String
    , key : Navigation.Key
    , data : Maybe AppData
    , dataNext : Maybe AppDataNext
    }


type Model
    = ReadyModel ReadyModelData
    | InitModel InitModelData
    | InitErrorModel Http.Error



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case model of
        InitModel initModel ->
            case message of
                SetData result ->
                    case result of
                        Err err ->
                            ( InitErrorModel err, Cmd.none )

                        Ok data ->
                            let
                                dataNext =
                                    convertData data

                                newModel =
                                    InitModel { initModel | data = Just data, dataNext = Just dataNext }
                            in
                            case allFieldsPresent newModel of
                                Just readyModelData ->
                                    ( ReadyModel readyModelData, Cmd.none )

                                Nothing ->
                                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InitErrorModel _ ->
            case message of
                _ ->
                    ( model, Cmd.none )

        ReadyModel readyModelData ->
            case message of
                SelectSection sectionId ->
                    ReadyModel { readyModelData | uiData = (setActiveSection readyModelData.uiData sectionId) }
                        |> update GeneratePageData

                GeneratePageData ->
                    -- todo convert to result
                    generatePageData readyModelData.dataNext readyModelData.uiData
                        |> Maybe.map (\page -> { readyModelData | page = page })
                        |> Maybe.map (\newModel -> ( ReadyModel newModel, Cmd.none ))
                        |> Maybe.withDefault
                            ( ReadyModel readyModelData, Cmd.none )

                DragStart tagId itemId _ _ ->
                    ReadyModel { readyModelData | uiData = (startDnD readyModelData.uiData tagId itemId) }
                        |> update GeneratePageData

                DragOver _ _ _ _ _ ->
                        ( ReadyModel readyModelData, Cmd.none )


                DragEnter tagId itemId dropTargetPosition _ ->
                   ReadyModel { readyModelData | uiData = (addDragOver readyModelData.uiData tagId itemId dropTargetPosition) }
                        |> update GeneratePageData

                DragLeave _ _ _ _ ->
                   ReadyModel { readyModelData | uiData = (removeDragOver readyModelData.uiData) }
                   |> update GeneratePageData

                DragEnd _ _ _ ->
                    ReadyModel { readyModelData | uiData = (readyModelData.uiData |> removeDragOver |> stopDnD ) }
                        |> update GeneratePageData

                Drop targetTagId targetItemId dropTargetPosition ->
                    let
                        _ = Debug.log "targetTagId" targetTagId
                        _ = Debug.log "targetItemId" targetItemId
                        _ = Debug.log "dropTargetPosition" dropTargetPosition
                        _ = Debug.log "source" readyModelData.uiData.dnd

                        newUiData = (readyModelData.uiData |> removeDragOver |> stopDnD )


                        maybeSourceItem =
                            Dict.get targetTagId readyModelData.dataNext.orderLists
                            |> Maybe.map2 (\(sourceTagId, sourceItemId) itemOrderList ->
                                case sourceTagId == targetTagId of
                                    True ->
                                        --TODO
                                        itemOrderList
                                    False ->
                                        itemOrderList
                            ) readyModelData.uiData.dnd


                    in
                    ( ReadyModel readyModelData, Cmd.none )
                    --case readyModelData.page of
                    --    SectionPage sectionPageData ->
                    --        let
                    --            maybeSectionData =
                    --                Utils.find
                    --                    (\section ->
                    --                        case section of
                    --                            GalleryWithTagsSectionType { sectionId } ->
                    --                                sectionId == sectionPageData.activeSectionId
                    --
                    --                            _ ->
                    --                                False
                    --                    )
                    --                    readyModelData.data
                    --                    ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")
                    --                    |> Result.andThen
                    --                        (\section ->
                    --                            case section of
                    --                                GalleryWithTagsSectionType sectionData ->
                    --                                    Ok sectionData
                    --
                    --                                _ ->
                    --                                    Err ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")
                    --                        )
                    --
                    --            maybeNewAppDataAndPage =
                    --                readyModelData.dnd
                    --                    |> Result.fromMaybe "DnD data not found"
                    --                    |> Result.andThen (\( sourceTagId, sourceItemId ) -> dndMove sourceTagId sourceItemId targetTagId targetItemId dropTargetPosition sectionPageData readyModelData.data)
                    --        in
                    --        case maybeNewAppDataAndPage of
                    --            Ok ( tempAppData, page ) ->
                    --                let
                    --                    appDataStringified =
                    --                        encodeAppData tempAppData
                    --                            |> debugLog "json:"
                    --
                    --                    saveToApi =
                    --                        Http.post
                    --                            { url = "bla"
                    --                            , body = Http.jsonBody appDataStringified
                    --                            , expect = expectJson SetData appDataDecoder
                    --                            }
                    --                in
                    --                ( ReadyModel { readyModelData | page = page, tempData = Just tempAppData, dnd = Nothing }, saveToApi )
                    --
                    --            Err errMessage ->
                    --                ( ReadyModel readyModelData, Cmd.none )
                    --
                    --    InitialPage _ ->
                    --        ( ReadyModel readyModelData, Cmd.none )

                SetData result ->
                    case result of
                        Err err ->
                            ( ReadyModel readyModelData, Cmd.none )

                        Ok data ->
                            ( ReadyModel readyModelData, Cmd.none )

                UrlChanged url ->
                    ( ReadyModel readyModelData, Cmd.none )

                _ ->
                    ( model, Cmd.none )


dndMove : TagId -> ItemId -> TagId -> ItemId -> DropTargetPosition -> SectionPageData -> AppData -> Result String ( AppData, Page )
dndMove sourceTagId sourceItemId targetTagId targetItemId dropTargetPosition sectionPageData appData =
    let
        maybeSectionIndex =
            Utils.findIndex
                (\section ->
                    case section of
                        GalleryWithTagsSectionType { sectionId } ->
                            sectionId == sectionPageData.activeSectionId

                        _ ->
                            False
                )
                appData
                ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")

        maybeSectionData =
            maybeSectionIndex
                |> Result.andThen
                    (\i ->
                        getAt i appData
                            |> Result.fromMaybe ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")
                    )
                |> Result.andThen
                    (\section ->
                        case section of
                            GalleryWithTagsSectionType sectionData ->
                                Ok sectionData

                            _ ->
                                Err ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")
                    )

        maybeTargetTagData =
            maybeSectionData
                |> Result.andThen
                    (\data ->
                        Utils.find
                            (\tagData -> tagData.tagId == targetTagId)
                            data.tags
                            ("Could not find tag with id " ++ targetTagId)
                    )

        maybeTargetTagViewData =
            Utils.find
                (\tagData -> tagData.tagId == targetTagId)
                sectionPageData.tags
                ("Could not find tag page data with id " ++ targetTagId)

        maybeTargetIndex =
            maybeTargetTagData
                |> Result.andThen
                    (\tagData ->
                        Utils.findIndex
                            (\{ itemId } -> itemId == targetItemId)
                            tagData.items
                            ("Could not find an item with id " ++ targetItemId)
                    )
                |> Result.map
                    (\i ->
                        case dropTargetPosition of
                            Before ->
                                i

                            After ->
                                i + 1
                    )

        maybeTargetViewIndex =
            maybeTargetTagViewData
                |> Result.andThen
                    (\tagData ->
                        Utils.findIndex
                            (\{ itemId } -> itemId == targetItemId)
                            tagData.items
                            ("Could not find an item with id " ++ targetItemId)
                    )
                |> Result.map
                    (\i ->
                        case dropTargetPosition of
                            Before ->
                                i

                            After ->
                                i + 1
                    )

        maybeNewViewItems =
            case sourceTagId == targetTagId of
                True ->
                    maybeTargetTagViewData
                        |> Result.andThen
                            (\tagData ->
                                Utils.findIndex
                                    (\item -> item.itemId == sourceItemId)
                                    tagData.items
                                    ("Could not find an item with id " ++ sourceItemId)
                                    |> Result.map (\i -> ( updateAt i (\item -> { item | isLoading = True }) tagData.items, i ))
                            )
                        |> Result.map2
                            (\targetIndex ( items, sourceIndex ) ->
                                move sourceIndex 1 targetIndex items
                            )
                            maybeTargetViewIndex

                False ->
                    Utils.find
                        (\tagData -> tagData.tagId == sourceTagId)
                        sectionPageData.tags
                        ("Could not find a tag with id " ++ sourceTagId)
                        |> Result.andThen
                            (\{ items } ->
                                Utils.find
                                    (\{ itemId } -> itemId == sourceItemId)
                                    items
                                    ("Could not find an item with id " ++ sourceItemId)
                            )
                        |> Result.map (\item -> { item | isLoading = True })
                        |> Result.map3
                            (\tagData targetIndex item ->
                                Utils.insert targetIndex tagData.items item
                            )
                            maybeTargetTagViewData
                            maybeTargetViewIndex

        maybeNewItems =
            case sourceTagId == targetTagId of
                True ->
                    maybeTargetTagData
                        |> Result.andThen
                            (\tagData ->
                                Utils.findIndex
                                    (\item -> item.itemId == sourceItemId)
                                    tagData.items
                                    ("Could not find an item with id " ++ sourceItemId)
                            )
                        |> Result.map3
                            (\tagData targetIndex sourceIndex ->
                                move sourceIndex 1 targetIndex tagData.items
                            )
                            maybeTargetTagData
                            maybeTargetIndex

                False ->
                    maybeSectionData
                        |> Result.andThen
                            (\{ tags } ->
                                Utils.find
                                    (\tagData -> tagData.tagId == sourceTagId)
                                    tags
                                    ("Could not find a tag with id " ++ sourceTagId)
                            )
                        |> Result.andThen
                            (\{ items } ->
                                Utils.find
                                    (\{ itemId } -> itemId == sourceItemId)
                                    items
                                    ("Could not find an item with id " ++ sourceItemId)
                            )
                        |> Result.map3
                            (\tagData targetIndex item ->
                                Utils.insert targetIndex tagData.items item
                            )
                            maybeTargetTagData
                            maybeTargetIndex

        maybeAppData =
            Result.map2 (\tag items -> { tag | items = items }) maybeTargetTagData maybeNewItems
                |> Result.map2 (\sectionData newTag -> setIf (\tag -> newTag.tagId == tag.tagId) newTag sectionData.tags) maybeSectionData
                |> Result.map2 (\sectionData newTags -> { sectionData | tags = newTags }) maybeSectionData
                |> Result.map (\sectionData -> GalleryWithTagsSectionType sectionData)
                |> Result.map2 (\i section -> ( i, section )) maybeSectionIndex
                |> Result.map (\( i, section ) -> setAt i section appData)

        maybeNewPage =
            Result.map2 (\newItems tagData -> { tagData | items = newItems }) maybeNewViewItems maybeTargetTagViewData
                |> Result.map (\newTagData -> setIf (\{ tagId } -> tagId == newTagData.tagId) newTagData sectionPageData.tags)
                |> Result.map (\newTags -> SectionPage { sectionPageData | tags = newTags })
    in
    Result.map2 (\newAppData newPage -> ( newAppData, newPage )) maybeAppData maybeNewPage


allFieldsPresent : Model -> Maybe ReadyModelData
allFieldsPresent newModel =
    case newModel of
        InitModel data ->
            let uiData = { view = Initial, imageUrl = data.imageUrl, dnd = Nothing, dragOver = Nothing }
            in
            data.dataNext
                |> Maybe.andThen
                    (\d -> generateMenuViewData d uiData sectionDataToSectionViewData)
                |> Maybe.map
                    (\sections -> Page.InitialPageData sections)
                |> Maybe.map (\s -> InitialPage s)
                |> Maybe.map3 (\d dn page -> { uiData = uiData, data = d, tempData = Nothing, apiUrl = data.imageUrl, key = data.key, page = page, dnd = Nothing, dataNext = dn }) data.data data.dataNext

        _ ->
            Nothing



-- MAIN --


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { apiBaseUrl, dataPath, imagePath, apiPort, apiProtocol } url key =
    let
        baseUrl =
            apiProtocol
                ++ "://"
                ++ apiBaseUrl
                ++ apiPort
                ++ "/"
    in
    ( InitModel (InitModelData url (baseUrl ++ imagePath ++ "/") key Nothing Nothing)
    , Cmd.batch
        [ Http.get
            { url = baseUrl ++ dataPath
            , expect = expectJson SetData appDataDecoder
            }
        , Task.perform SetViewport getViewport
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize (\_ _ -> GetViewport)



-- VIEW --


view : Model -> Browser.Document Msg
view model =
    case model of
        InitModel _ ->
            Browser.Document "** palavara **" initPage

        InitErrorModel _ ->
            Browser.Document "** palavara **" [ text "error" ]

        ReadyModel { page } ->
            Browser.Document "** palavara **" <| contentPage page


initPage =
    [ div [ class "loader" ]
        [ div [ class "logo-image" ]
            [ Icons.logo ]
        ]
    ]


contentPage : Page -> List (Html Msg)
contentPage page =
    case page of
        InitialPage { sections } ->
            [ div [ class "layout" ]
                [ lazy menu sections
                ]
            ]

        SectionPage { sections, activeSectionId, tags, items } ->
            [ div [ class "layout" ]
                [ lazy menu sections
                , div [ class "section" ]
                    [ div [ class "sectionTitle" ] [ text activeSectionId ]
                    , node "div" [ class "items" ] (itemList items)
                    , div [ class "tags" ] (tagList tags)
                    ]
                ]
            ]


menu : List ( SectionId, String ) -> Html Msg
menu sections =
    div [ class "menu" ] <|
        List.map
            (\( sectionId, label ) ->
                div [ class "menu-section" ]
                    [ div [ onClickPreventDefault (SelectSection sectionId) ] [ text label ]
                    ]
            )
            sections


tagList : List TagViewData -> List (Html Msg)
tagList tagData =
    List.map
        (\{ label, items, tagId } ->
            div [ class "tag" ]
                [ text label
                , lazy2 itemRow
                    items
                    tagId
                ]
        )
        tagData


itemRow items tagId =
    node "div" [ class "items" ] (dropzoneItemList items tagId)


dropzoneItemList items tagId =
    List.indexedMap
        (\i itemData ->
            ( itemData.itemId
            , lazy2 dropzoneItemView itemData tagId
            )
        )
        items


dropzoneItemView itemData tagId =
    div (class "item" :: onSourceDrag { effectAllowed = { move = True, copy = False, link = False }, onStart = DragStart tagId itemData.itemId, onEnd = DragEnd tagId itemData.itemId, onDrag = Nothing })
        [ div (class ("item-dropzone-left dropTarget" ++ isActive Before itemData) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver tagId itemData.itemId Before, onDrop = always (Drop tagId itemData.itemId Before), onEnter = Just (DragEnter tagId itemData.itemId Before), onLeave = Just (DragLeave tagId itemData.itemId Before) }) []
        , div [ class ("item-internal dnd " ++ isLoading itemData), id itemData.itemId ] [ img [ src itemData.src, alt itemData.fileName ] [] ]
        , div (class ("item-dropzone-right dropTarget" ++ isActive After itemData) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver tagId itemData.itemId After, onDrop = always (Drop tagId itemData.itemId After), onEnter = Just (DragEnter tagId itemData.itemId After), onLeave = Just (DragLeave tagId itemData.itemId After) }) []
        ]


isLoading itemData =
    case itemData.isLoading of
        True ->
            "loading"

        False ->
            ""


isActive dropTargetPosition itemData =
    case itemData.dndOnOver of
        Just position ->
                case positionIsEqual position dropTargetPosition of
                    True -> " active"
                    False -> ""
        Nothing ->
            ""

itemList items =
    List.indexedMap
        (\i itemData ->
            ( itemData.itemId
            , lazy itemView itemData
            )
        )
        items


itemView itemData =
    div [ class "item" ]
        [ div [ class "item-dropzone-left" ] []
        , div [ class "item-internal dnd" ] [ img [ src itemData.src, alt itemData.fileName ] [] ]
        , div [ class "item-dropzone-right" ] []
        ]


debugLog s x =
    let
        _ =
            Debug.log s x
    in
    x
