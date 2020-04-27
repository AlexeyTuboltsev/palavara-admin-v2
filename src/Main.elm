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
import List.Extra exposing (find, findIndex, getAt, removeAt, setIf, splitAt, unique)
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
    , dnd : Maybe (TagId,ItemId)
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
                                                        ItemViewData itemData.itemId itemData.fileName itemData.urlString (readyModelData.apiUrl ++ itemData.fileName) False

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

                DragStart tagId itemId effectAllowed value ->
                    let
                        newPage =
                            case readyModelData.page of
                                InitialPage _ ->
                                    readyModelData.page

                                SectionPage sectionPageData ->
                                    SectionPage { sectionPageData | dnd = Just (tagId,itemId) }
                    in
                    ( ReadyModel { readyModelData | page = newPage, dnd = Just (tagId,itemId) }, Cmd.none )

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
                            let maybeNewPage = readyModelData.dnd
                                    |> Maybe.andThen (\(sourceTagId, sourceItemId) -> dndMove sourceTagId sourceItemId targetTagId targetItemId dropTargetPosition sectionPageData)


                            in
                            case maybeNewPage of
                                Just page ->
                                    ( ReadyModel { readyModelData | page = page, dnd = Nothing }, Cmd.none )

                                Nothing ->
                                    ( ReadyModel readyModelData, Cmd.none )

                        InitialPage _ ->
                            ( ReadyModel readyModelData, Cmd.none )

                UrlChanged url ->
                    ( ReadyModel readyModelData, Cmd.none )

                _ ->
                    ( model, Cmd.none )


dndMove sourceTagId sourceItemId targetTagId targetItemId dropTargetPosition sectionPageData =
    let

        maybeActiveTagViewData =
            find (\tagData -> tagData.tagId == targetTagId) sectionPageData.tags

        maybeTargetIndex =
            maybeActiveTagViewData
                |> Maybe.andThen (\( tagData ) ->
                             findIndex (\item -> item.itemId == targetItemId) tagData.items
                         )
                 |> Maybe.map (\i -> case dropTargetPosition of
                        Before -> i
                        After -> i+1
                 )

        maybeSourceIndex =
            maybeActiveTagViewData
            |> Maybe.andThen (\tagData ->findIndex (\item -> item.itemId == sourceItemId) tagData.items)


        maybeNewItems =
            Maybe.map3 (\tagData sourceIndex targetIndex -> move sourceIndex 1 targetIndex tagData.items) maybeActiveTagViewData maybeSourceIndex maybeTargetIndex

        maybeNewPage =
            maybeActiveTagViewData
                |> Maybe.map2 (\newItems tagData -> { tagData | items = newItems }) maybeNewItems
                |> Maybe.map (\newTagData -> setIf (\{ tagId } -> tagId == newTagData.tagId) newTagData sectionPageData.tags)
                |> Maybe.map (\newTags -> SectionPage { sectionPageData | tags = newTags })
    in
    maybeNewPage

move originalPositionIndex originalLength targetPositionIndex list =
    let
        _ = Debug.log "originalPositionIndex + originalLength" (originalPositionIndex + originalLength)
        _ = Debug.log "(List.length list) + 1" ((List.length list))
    in
    if (originalPositionIndex < targetPositionIndex)
        && ((originalPositionIndex + originalLength) > targetPositionIndex) then
        list

    else if (originalPositionIndex + originalLength) > (List.length list) || originalPositionIndex < 0 then
        list

    else if targetPositionIndex > (List.length list) || targetPositionIndex < 0 then
        list

    else
    split [originalPositionIndex,(originalPositionIndex + originalLength), targetPositionIndex] list
        |> debugLog "split"
        |> (\acc ->
            let _ = Debug.log "acc" acc
            in
            case acc of
                [a,b,c,d] ->

                    List.concat [a,c,b,d]
                _ ->
                     []
        )



split points list =
    let _ = Debug.log "---bla" list
    in
    points
        |> unique
        |> List.sort
        |> List.foldr
            (\point ( rest, acc ) ->
                splitAt point rest
                    |> (\( nextRest, x ) -> ( nextRest, x :: acc ))
            )
            ( list, [] )
        |> (\(last, acc) -> last::acc)


insert dropTargetPosition itemIndex list item =
    case dropTargetPosition of
        Before ->
            insertBefore list itemIndex item

        After ->
            insertAfter list itemIndex item


insertBefore list itemIndex item =
    case itemIndex of
        0 ->
            item :: list

        _ ->
            let
                ( a, b ) =
                    splitAt itemIndex list
            in
            List.concat [ a, [ item ], b ]


insertAfter list itemIndex item =
    if (itemIndex + 1) == List.length list then
        List.append list [ item ]

    else
        let
            ( a, b ) =
                splitAt (itemIndex + 1) list
        in
        List.concat [ a, [ item ], b ]


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


tagList : List TagViewData -> Maybe (tagId, ItemId) -> Maybe ( TagId, ItemId, DropTargetPosition ) -> List (Html Msg)
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
    div (class "item" :: onSourceDrag { effectAllowed = { move = True, copy = False, link = False }, onStart = DragStart tagId itemData.itemId, onEnd = DragEnd tagId itemData.itemId, onDrag = Nothing })
        [ div (class ("item-dropzone-left dropTarget" ++ isActive maybeDndItemId itemData.itemId Before) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop tagId itemData.itemId Before), onEnter = Just (DragEnter tagId itemData.itemId Before), onLeave = Just (DragLeave tagId itemData.itemId Before) }) []
        , div [ class "item-internal dnd" ] [ img [ src itemData.src, alt itemData.fileName ] [] ]
        , div (class ("item-dropzone-right dropTarget" ++ isActive maybeDndItemId itemData.itemId After) :: onDropTarget { dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop tagId itemData.itemId After), onEnter = Just (DragEnter tagId itemData.itemId After), onLeave = Just (DragLeave tagId itemData.itemId After) }) []
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


debugLog s x =
    let
        _ =
            Debug.log s x
    in
    x
