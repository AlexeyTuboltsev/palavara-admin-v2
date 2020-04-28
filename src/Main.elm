module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation exposing (Key, load)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (alt, class, href, id, property, src, style)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Events.Extra.Drag exposing (DropEffect(..), onDropTarget, onSourceDrag)
import Html.Keyed exposing (node)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Http exposing (expectJson, get)
import Icons
import List.Extra as LE exposing (find, findIndex, getAt, removeAt, setAt, setIf, splitAt, unique, updateAt)
import Message exposing (Msg(..))
import Page exposing (ItemViewData, Page(..), SectionPageData, TagViewData)
import Result exposing (Result)
import Task
import Url exposing (Url)
import Utils exposing (find, findIndex, insert, move, positionIsEqual)


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
    , dnd : Maybe ( TagId, ItemId )
    }


type alias InitModelData =
    { url : Url
    , dataUrl : String
    , key : Navigation.Key
    , data : Maybe AppData
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
                                newModel =
                                    InitModel { initModel | data = Just data }
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
                    let
                        sectionData =
                            getSectionData readyModelData.data (\s -> ( s.sectionId, s.label ))

                        maybeNewModel =
                            Utils.find
                                (\section ->
                                    case section of
                                        GalleryWithTagsSectionType sd ->
                                            sd.sectionId == sectionId

                                        _ ->
                                            False
                                )
                                readyModelData.data
                                ("Could not find a section with id " ++ sectionId)
                                |> Result.andThen
                                    (\s ->
                                        case s of
                                            GalleryWithTagsSectionType sd ->
                                                let
                                                    generateItemViewData itemData =
                                                        ItemViewData itemData.itemId itemData.fileName itemData.urlString (readyModelData.apiUrl ++ itemData.fileName) False

                                                    generateTagViewData tagData =
                                                        TagViewData tagData.label tagData.tagId (List.map generateItemViewData tagData.items)

                                                    tags =
                                                        List.map generateTagViewData sd.tags

                                                    items =
                                                        List.map generateItemViewData sd.items
                                                in
                                                Ok ( tags, items )

                                            _ ->
                                                Err ("Wrong section type for section with id " ++ sectionId)
                                    )
                                |> Result.map (\( tags, items ) -> SectionPage { activeSectionId = sectionId, sections = sectionData, tags = tags, items = items, dnd = readyModelData.dnd, dragOver = Nothing })
                                |> Result.map (\page -> { readyModelData | page = page })
                    in
                    case maybeNewModel of
                        Ok nm ->
                            ( ReadyModel nm, Cmd.none )

                        Err errMessage ->
                            ( ReadyModel readyModelData, Cmd.none )

                DragStart tagId itemId _ _ ->
                    let
                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dnd = Just ( tagId, itemId ) }
                    in
                    ( ReadyModel { readyModelData | page = newPage, dnd = Just ( tagId, itemId ) }, Cmd.none )

                DragOver _ _ ->
                    ( ReadyModel readyModelData, Cmd.none )

                DragEnter tagId itemId dropTargetPosition _ ->
                    let
                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dragOver = Just ( tagId, itemId, dropTargetPosition ) }
                    in
                    ( ReadyModel { readyModelData | page = newPage }, Cmd.none )

                DragLeave tagId itemId dropTargetPosition event ->
                    let
                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dragOver = Nothing }
                    in
                    ( ReadyModel { readyModelData | page = newPage }, Cmd.none )

                DragEnd _ _ _ ->
                    let
                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dnd = Nothing, dragOver = Nothing }

                        _ =
                            Debug.log "-->DragEnd" ""
                    in
                    ( ReadyModel { readyModelData | page = newPage, dnd = Nothing }, Cmd.none )

                Drop targetTagId targetItemId dropTargetPosition ->
                    case readyModelData.page of
                        SectionPage sectionPageData ->
                            let
                                maybeSectionData =
                                    Utils.find
                                        (\section ->
                                            case section of
                                                GalleryWithTagsSectionType { sectionId } ->
                                                    sectionId == sectionPageData.activeSectionId

                                                _ ->
                                                    False
                                        )
                                        readyModelData.data
                                        ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")
                                        |> Result.andThen
                                            (\section ->
                                                case section of
                                                    GalleryWithTagsSectionType sectionData ->
                                                        Ok sectionData

                                                    _ ->
                                                        Err ("Could not find section with id " ++ sectionPageData.activeSectionId ++ " in appData")
                                            )

                                maybeNewAppDataAndPage =
                                    readyModelData.dnd
                                        |> Result.fromMaybe "DnD data not found"
                                        |> Result.andThen (\( sourceTagId, sourceItemId ) -> dndMove sourceTagId sourceItemId targetTagId targetItemId dropTargetPosition sectionPageData readyModelData.data)
                            in
                            case maybeNewAppDataAndPage of
                                Ok (tempAppData,page) ->
                                    let
                                        appDataStringified =
                                            encodeAppData tempAppData
                                            |> debugLog "json:"

                                        saveToApi =
                                            Http.post
                                                {url = "bla"
                                                , body = Http.jsonBody appDataStringified
                                                , expect = expectJson SetData appDataDecoder
                                                }

                                    in
                                    ( ReadyModel { readyModelData | page = page, tempData = Just tempAppData, dnd = Nothing }, saveToApi )

                                Err errMessage ->
                                    ( ReadyModel readyModelData, Cmd.none )

                        InitialPage _ ->
                            ( ReadyModel readyModelData, Cmd.none )
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


dndMove : TagId -> ItemId -> TagId -> ItemId -> DropTargetPosition -> SectionPageData -> AppData -> Result String (AppData,Page)
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
                                |> Result.map (\i -> (updateAt i (\item -> { item | isLoading = True }) tagData.items,i))
                            )
                        |> Result.map2
                            (\targetIndex (items,sourceIndex) ->
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
                                insert targetIndex tagData.items item
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
                            (\{tags} ->
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
                                insert targetIndex tagData.items item
                            )
                            maybeTargetTagData
                            maybeTargetIndex

        maybeAppData =
            Result.map2 (\tag items -> {tag | items = items}) maybeTargetTagData maybeNewItems
                |> Result.map2 (\sectionData newTag -> setIf (\tag -> newTag.tagId == tag.tagId) newTag sectionData.tags) maybeSectionData
                |> Result.map2 (\sectionData newTags -> {sectionData| tags = newTags}) maybeSectionData
                |> Result.map (\sectionData -> GalleryWithTagsSectionType sectionData)
                |> Result.map2 (\i section -> (i,section)) maybeSectionIndex
                |> Result.map (\(i,section) -> setAt i section appData)
        maybeNewPage =
                Result.map2 (\newItems tagData -> { tagData | items = newItems }) maybeNewViewItems maybeTargetTagViewData
                    |> Result.map (\newTagData -> setIf (\{ tagId } -> tagId == newTagData.tagId) newTagData sectionPageData.tags)
                    |> Result.map (\newTags -> SectionPage { sectionPageData | tags = newTags })
    in
    Result.map2 (\newAppData newPage -> (newAppData, newPage)) maybeAppData maybeNewPage



allFieldsPresent : Model -> Maybe ReadyModelData
allFieldsPresent newModel =
    case newModel of
        InitModel data ->
            Maybe.map
                (\d ->
                    let
                        page =
                            getSectionData d (\s -> ( s.sectionId, s.label ))
                                |> Page.InitialPageData
                                |> InitialPage
                    in
                    { data = d, tempData = Nothing, apiUrl = data.dataUrl, key = data.key, page = page, dnd = Nothing }
                )
                data.data

        _ ->
            Nothing


getSectionData appData convertFn =
    List.filterMap
        (\section ->
            case section of
                GalleryWithTagsSectionType s ->
                    Just (convertFn s)

                _ ->
                    Nothing
        )
        appData



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
    ( InitModel (InitModelData url (baseUrl ++ imagePath ++ "/") key Nothing)
    , Cmd.batch
        [ get
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

        SectionPage { sections, activeSectionId, tags, items, dnd, dragOver } ->
            [ div [ class "layout" ]
                [ lazy menu sections
                , div [ class "section" ]
                    [ div [ class "sectionTitle" ] [ text activeSectionId ]
                    , node "div" [ class "items" ] (itemList items)
                    , div [ class "tags" ] (tagList tags dnd dragOver)
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


tagList : List TagViewData -> Maybe ( tagId, ItemId ) -> Maybe ( TagId, ItemId, DropTargetPosition ) -> List (Html Msg)
tagList tagData dnd maybeDragOver =
    List.map
        (\{ label, items, tagId } ->
            div [ class "tag" ]
                [ text label
                , lazy3 itemRow
                    items
                    tagId
                    (Maybe.andThen
                        (\( dndTagId, dndItemId, position ) ->
                            if dndTagId == tagId then
                                Just ( dndItemId, position )

                            else
                                Nothing
                        )
                        maybeDragOver
                    )
                ]
        )
        tagData


itemRow items tagId maybeDnd =
    node "div" [ class "items" ] (dropzoneItemList items tagId maybeDnd)


dropzoneItemList items tagId maybeDndItemId =
    List.indexedMap
        (\i itemData ->
            ( itemData.itemId
            , lazy3 dropzoneItemView itemData tagId maybeDndItemId
            )
        )
        items


dropzoneItemView itemData tagId maybeDndItemId =
    div (class "item" :: onSourceDrag { effectAllowed = { move = True, copy = False, link = False }, onStart = DragStart tagId itemData.itemId, onEnd = DragEnd tagId itemData.itemId, onDrag = Nothing })
        [ div (class ("item-dropzone-left dropTarget" ++ isActive maybeDndItemId itemData.itemId Before) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop tagId itemData.itemId Before), onEnter = Just (DragEnter tagId itemData.itemId Before), onLeave = Just (DragLeave tagId itemData.itemId Before) }) []
        , div [ class ("item-internal dnd " ++ isLoading itemData) ] [ img [ src itemData.src, alt itemData.fileName ] [] ]
        , div (class ("item-dropzone-right dropTarget" ++ isActive maybeDndItemId itemData.itemId After) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop tagId itemData.itemId After), onEnter = Just (DragEnter tagId itemData.itemId After), onLeave = Just (DragLeave tagId itemData.itemId After) }) []
        ]


isLoading itemData =
    case itemData.isLoading of
        True ->
            "loading"

        False ->
            ""


isActive maybeDndItemId itemId position =
    case maybeDndItemId of
        Nothing ->
            ""

        Just ( dndItemId, dndPosition ) ->
            if dndItemId == itemId && positionIsEqual dndPosition position then
                " active"

            else
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
