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
import List.Extra exposing (find)
import Message exposing (Msg(..))
import Page exposing (ItemViewData, Page(..), TagViewData)
import Result exposing (Result)
import Task
import Url exposing (Url)


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
    , dnd : Maybe ItemId
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
                            find
                                (\section ->
                                    case section of
                                        GalleryWithTagsSectionType sd ->
                                            sd.sectionId == sectionId

                                        _ ->
                                            False
                                )
                                readyModelData.data
                                |> Maybe.andThen
                                    (\s ->
                                        case s of
                                            GalleryWithTagsSectionType sd ->
                                                let
                                                    generateItemViewData itemData =
                                                        ItemViewData itemData.itemId itemData.fileName itemData.urlString (readyModelData.apiUrl ++ itemData.fileName)

                                                    generateTagViewData tagData =
                                                        TagViewData tagData.label tagData.tagId (List.map generateItemViewData tagData.items)

                                                    tags =
                                                        List.map generateTagViewData sd.tags

                                                    items =
                                                        List.map generateItemViewData sd.items
                                                in
                                                Just ( tags, items )

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.map (\( tags, items ) -> SectionPage { activeSectionId = sectionId, sections = sectionData, tags = tags, items = items, dnd = readyModelData.dnd, dragOver = Nothing })
                                |> Maybe.map (\page -> { readyModelData | page = page })
                    in
                    case maybeNewModel of
                        Just nm ->
                            ( ReadyModel nm, Cmd.none )

                        Nothing ->
                            ( ReadyModel readyModelData, Cmd.none )

                DragStart itemId effectAllowed value ->
                    let
                        _ =
                            Debug.log "dragstart itemId" itemId

                        _ =
                            Debug.log "dragstart effectAllowed" effectAllowed

                        _ =
                            Debug.log "dragstart value" value

                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dnd = Just itemId }
                    in
                    ( ReadyModel { readyModelData | page = newPage, dnd = Just itemId }, Cmd.none )

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

                DragEnd _ _ event ->
                    let
                        _ =
                            Debug.log "DragEnd" event

                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dnd = Nothing, dragOver = Nothing }
                    in
                    ( ReadyModel { readyModelData | page = newPage, dnd = Nothing }, Cmd.none )

                Drop itemId dropTargetPosition ->
                    let
                        _ =
                            Debug.log "Drop itemId" itemId

                        _ =
                            Debug.log "Drop dropTargetPosition" dropTargetPosition
                    in
                    ( ReadyModel readyModelData, Cmd.none )

                UrlChanged url ->
                    ( ReadyModel readyModelData, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
                    { data = d, apiUrl = data.dataUrl, key = data.key, page = page, dnd = Nothing }
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


tagList : List TagViewData -> Maybe ItemId -> Maybe ( TagId, ItemId, DropTargetPosition ) -> List (Html Msg)
tagList tagData dnd maybeDragOver =
    List.map
        (\{ label, items, tagId } ->
            div [ class "tag" ]
                [ text label
                , lazy4 itemRow
                    items
                    tagId
                    dnd
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


itemRow items tagId dnd maybeDndItemId =
    node "div" [ class "items" ] (dropzoneItemList items tagId maybeDndItemId)


dropzoneItemList items tagId maybeDndItemId =
    List.indexedMap
        (\i itemData ->
            ( itemData.itemId
            , lazy3 dropzoneItemView itemData tagId maybeDndItemId
            )
        )
        items


dropzoneItemView itemData tagId maybeDndItemId =
    div (class "item" :: onSourceDrag { effectAllowed = { move = True, copy = False, link = False }, onStart = DragStart itemData.itemId, onEnd = DragEnd tagId itemData.itemId, onDrag = Nothing })
        [ div (class ("item-dropzone-left dropTarget" ++ isActive maybeDndItemId itemData.itemId Before) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop itemData.itemId Before), onEnter = Just (DragEnter tagId itemData.itemId Before), onLeave = Just (DragLeave tagId itemData.itemId Before) }) []
        , div [ class "item-internal dnd" ] [ img [ src itemData.src, alt itemData.fileName ] [] ]
        , div (class ("item-dropzone-right dropTarget" ++ isActive maybeDndItemId itemData.itemId After) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop itemData.itemId After), onEnter = Just (DragEnter tagId itemData.itemId After), onLeave = Just (DragLeave tagId itemData.itemId After) }) []
        ]


isActive maybeDndItemId itemId position =
    case maybeDndItemId of
        Nothing ->
            ""

        Just ( dndItemId, dndPosition ) ->
            if dndItemId == itemId && positionIsEqual dndPosition position then
                " active"

            else
                ""


positionIsEqual x y =
    case ( x, y ) of
        ( Before, Before ) ->
            True

        ( After, After ) ->
            True

        _ ->
            False


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



--
--debugLog s x =
--    let
--        _ =
--            Debug.log s x
--    in
--    x
