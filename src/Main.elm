module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation exposing (Key, load)
import Dict exposing (..)
import Html exposing (Html, a, br, div, h2, img, span, text)
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
import Page exposing (ItemViewData, Modal(..), Page(..), SectionPageData, TagViewData, UIData, View(..), addDragOver, generateMenuViewData, generatePageData, removeDragOver, sectionDataToSectionViewData, setActiveSection, startDnD, stopDnD)
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
                    ReadyModel { readyModelData | uiData = setActiveSection readyModelData.uiData sectionId }
                        |> update GeneratePageData

                GeneratePageData ->
                    -- todo convert to result
                    generatePageData readyModelData.dataNext readyModelData.uiData
                        |> Maybe.map (\page -> { readyModelData | page = page })
                        |> Maybe.map (\newModel -> ( ReadyModel newModel, Cmd.none ))
                        |> Maybe.withDefault
                            ( ReadyModel readyModelData, Cmd.none )

                DragStart tagId itemId _ _ ->
                    ReadyModel { readyModelData | uiData = startDnD readyModelData.uiData tagId itemId }
                        |> update GeneratePageData

                DragOver _ _ ->
                    ( ReadyModel readyModelData, Cmd.none )

                DragEnter tagId itemId dropTargetPosition _ ->
                    ReadyModel { readyModelData | uiData = addDragOver readyModelData.uiData tagId itemId dropTargetPosition }
                        |> update GeneratePageData

                DragLeave _ _ _ _ ->
                    ReadyModel { readyModelData | uiData = removeDragOver readyModelData.uiData }
                        |> update GeneratePageData

                DragEnd _ _ _ ->
                    ReadyModel { readyModelData | uiData = readyModelData.uiData |> removeDragOver |> stopDnD }
                        |> update GeneratePageData

                Drop targetTagId targetItemId dropTargetPosition ->
                    let
                        newUiData =
                            readyModelData.uiData |> removeDragOver |> stopDnD

                        maybeItemOrderListId =
                            Dict.get targetTagId readyModelData.dataNext.tags
                                |> Maybe.map (\tag -> tag.itemOrderId)

                        maybeDataNext =
                            maybeItemOrderListId
                                |> Maybe.andThen (\itemOrderId -> Dict.get itemOrderId readyModelData.dataNext.orderLists)
                                |> Maybe.map2 (\( sourceTagId, sourceItemId ) itemOrderList -> ( sourceTagId, sourceItemId, itemOrderList )) readyModelData.uiData.dnd
                                |> Maybe.andThen
                                    (\( sourceTagId, sourceItemId, itemOrderList ) ->
                                        case sourceTagId == targetTagId of
                                            True ->
                                                let
                                                    maybeSourceItemIndex =
                                                        LE.findIndex (\id -> sourceItemId == id) itemOrderList

                                                    maybeTargetItemIndex =
                                                        Utils.findInsertionIndex itemOrderList targetItemId dropTargetPosition
                                                in
                                                Maybe.map2 (\sourceItemIndex targetItemIndex -> move sourceItemIndex 1 targetItemIndex itemOrderList) maybeSourceItemIndex maybeTargetItemIndex

                                            False ->
                                                let
                                                    filteredOrderList =
                                                        LE.remove sourceItemId itemOrderList

                                                    maybeTargetItemIndex =
                                                        Utils.findInsertionIndex filteredOrderList targetItemId dropTargetPosition
                                                in
                                                Maybe.map (\targetItemIndex -> Utils.insert targetItemIndex filteredOrderList sourceItemId) maybeTargetItemIndex
                                    )
                                |> Maybe.map2
                                    (\itemOrderListId newItemOrderList ->
                                        Dict.update itemOrderListId (\_ -> Just newItemOrderList) readyModelData.dataNext.orderLists
                                    )
                                    maybeItemOrderListId
                                |> Maybe.map
                                    (\orderLists ->
                                        let
                                            dataNext =
                                                readyModelData.dataNext
                                        in
                                        { dataNext | orderLists = orderLists }
                                    )
                    in
                    case maybeDataNext of
                        Just dn ->
                            ReadyModel { readyModelData | uiData = newUiData, dataNext = dn }
                                |> update GeneratePageData

                        Nothing ->
                            ( ReadyModel readyModelData, Cmd.none )

                MoveItemLeft tagId itemId ->
                    let
                        fn =
                            \itemIndex itemOrderList ->
                                case itemIndex of
                                    0 ->
                                        itemOrderList

                                    _ ->
                                        LE.swapAt itemIndex (itemIndex - 1) itemOrderList

                        maybeDataNext =
                            updateItemList readyModelData.dataNext tagId itemId fn
                    in
                    case maybeDataNext of
                        Just nd ->
                            ReadyModel { readyModelData | dataNext = nd }
                                |> update GeneratePageData

                        Nothing ->
                            ( ReadyModel readyModelData, Cmd.none )

                MoveItemRight tagId itemId ->
                    let
                        fn =
                            \itemIndex itemOrderList ->
                                case itemIndex == (List.length itemOrderList - 1) of
                                    True ->
                                        itemOrderList

                                    False ->
                                        LE.swapAt itemIndex (itemIndex + 1) itemOrderList

                        maybeDataNext =
                            updateItemList readyModelData.dataNext tagId itemId fn
                    in
                    case maybeDataNext of
                        Just nd ->
                            ReadyModel { readyModelData | dataNext = nd }
                                |> update GeneratePageData

                        Nothing ->
                            ( ReadyModel readyModelData, Cmd.none )

                AskToDeleteItem tagId itemId ->
                    let
                        newUiData =
                            readyModelData.uiData
                                |> (\uiData -> { uiData | modal = ConfirmDeleteItem tagId itemId })
                    in
                    ( ReadyModel { readyModelData | uiData = newUiData }, Cmd.none )

                CancelDeleteItem ->
                    let
                        newUiData =
                            readyModelData.uiData
                                |> (\uiData -> { uiData | modal = ModalClosed })
                    in
                    ( ReadyModel { readyModelData | uiData = newUiData }, Cmd.none )

                ConfirmDeleteItemFromTag tagId itemId ->
                    let
                        newUiData =
                            readyModelData.uiData
                                |> (\uiData -> { uiData | modal = ModalClosed })

                        newModel =
                            ReadyModel { readyModelData | uiData = newUiData }
                    in
                    update (DeleteItem tagId itemId) newModel

                DeleteItem tagId itemId ->
                    let
                        fn =
                            \itemIndex itemOrderList ->
                                LE.removeAt itemIndex itemOrderList

                        maybeDataNext =
                            updateItemList readyModelData.dataNext tagId itemId fn
                    in
                    case maybeDataNext of
                        Just nd ->
                            ReadyModel { readyModelData | dataNext = nd }
                                |> update GeneratePageData

                        Nothing ->
                            ( ReadyModel readyModelData, Cmd.none )

                EditItem itemId ->
                    let
                        itemEditorState =
                            Dict.get itemId readyModelData.dataNext.items
                                |> Maybe.map
                                    (\itemData ->
                                        { itemId = itemData.itemId
                                        , fileName = Just itemData.fileName
                                        , src = Just (readyModelData.uiData.imageUrl ++ itemData.fileName)
                                        , urlString = Just itemData.urlString
                                        , usedIn = itemData.usedIn
                                        }
                                    )
                                |> Maybe.map Page.ItemEditorOpen
                                |> Maybe.withDefault Page.ItemEditorClosed

                        newModel =
                            readyModelData.uiData
                                |> (\uiData -> { uiData | itemEditor = itemEditorState })
                                |> (\newUiData -> { readyModelData | uiData = newUiData })
                                |> ReadyModel
                    in
                    ( newModel, Cmd.none )

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


allFieldsPresent : Model -> Maybe ReadyModelData
allFieldsPresent newModel =
    case newModel of
        InitModel data ->
            let
                uiData =
                    { itemEditor = Page.ItemEditorClosed, view = Initial, imageUrl = data.imageUrl, dnd = Nothing, dragOver = Nothing, modal = ModalClosed }
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

        ReadyModel { page, uiData } ->
            Browser.Document "** palavara **" <| contentPage page uiData.modal uiData.itemEditor


initPage =
    [ div [ class "loader" ]
        [ div [ class "logo-image" ]
            [ Icons.logo ]
        ]
    ]


contentPage : Page -> Modal -> Page.ItemEditor -> List (Html Msg)
contentPage page modalData itemEditorData =
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
            , modal modalData
            , itemEditor itemEditorData
            ]


itemEditor data =
    case data of
        Page.ItemEditorClosed ->
            Utils.emptyHtml

        Page.ItemEditorOpen itemData ->
            let
                urlString =
                    itemData.urlString
                        |> Maybe.withDefault ""

                imageSrc =
                    itemData.src
                        |> Maybe.withDefault ""
            in
            div [ class "item-editor-wrapper" ]
                [ div [ class "item-editor" ]
                    [ h2 [ class "item-editor-title" ] [ text "EDIT ITEM" ]
                    , div [] [ text urlString ]
                    , img [ class "item-editor-image", src imageSrc, alt urlString ] []
                    , div [ class "modal-buttons" ] [
                        div [ class "button" ] [ text "cancel" ]
                    ]
                    ]
                ]


modal modalData =
    case modalData of
        Page.ModalClosed ->
            Utils.emptyHtml

        Page.ConfirmDeleteItem tagId itemId ->
            modalBase
                Nothing
                (Just "Are you sure?")
                [ { action = ConfirmDeleteItemFromTag tagId itemId, text = "delete" }, { action = CancelDeleteItem, text = "cancel" } ]


modalBase : Maybe String -> Maybe String -> List { action : Msg, text : String } -> Html Msg
modalBase maybeTitle maybeText buttons =
    let
        modalTitle =
            Maybe.map (\titleText -> div [ class "modal-title" ] [ text titleText ]) maybeTitle
                |> Maybe.withDefault Utils.emptyHtml

        modalText =
            Maybe.map (\txt -> div [ class "modal-text" ] [ text txt ]) maybeText
                |> Maybe.withDefault Utils.emptyHtml

        modalButtons =
            List.map
                (\buttonData ->
                    div [ class "button", onClickPreventDefault buttonData.action ] [ text buttonData.text ]
                )
                buttons
    in
    div [ class "modal-wrapper" ]
        [ div [ class "modal-background" ]
            []
        , div
            [ class "modal" ]
            [ modalTitle
            , modalText
            , div [ class "modal-buttons" ] modalButtons
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
    div (class ("item" ++ isDnD itemData) :: onSourceDrag { effectAllowed = { move = True, copy = False, link = False }, onStart = DragStart tagId itemData.itemId, onEnd = DragEnd tagId itemData.itemId, onDrag = Nothing })
        [ dropzoneLeft itemData tagId
        , div [ class ("item-internal-wrapper" ++ isDnD itemData), id itemData.itemId ]
            [ div [ class "item-internal" ]
                [ div [ class "item-button upper-left", onClickPreventDefault <| MoveItemLeft tagId itemData.itemId ] [ text "<=" ]
                , div [ class "item-button upper-right", onClickPreventDefault <| MoveItemRight tagId itemData.itemId ] [ text "=>" ]
                , img [ src itemData.src, alt itemData.fileName ] []
                , div [ class "item-button bottom-left", onClickPreventDefault <| EditItem itemData.itemId ] [ text "edit" ]
                , div [ class "item-button bottom-right", onClickPreventDefault <| AskToDeleteItem tagId itemData.itemId ] [ text "delete" ]
                ]
            ]
        , dropzoneRight itemData tagId
        ]


dropzoneLeft itemData tagId =
    div (class ("item-dropzone-left dropTarget" ++ isActive Before itemData) :: onDropTarget { dropEffect = NoDropEffect, onOver = DragOver, onDrop = always (Drop tagId itemData.itemId Before), onEnter = Just (DragEnter tagId itemData.itemId Before), onLeave = Just (DragLeave tagId itemData.itemId Before) })
        [ div [ class "dropzone-internal" ] [] ]


dropzoneRight itemData tagId =
    div (class ("item-dropzone-right dropTarget" ++ isActive After itemData) :: onDropTarget { dropEffect = NoDropEffect, onOver = DragOver, onDrop = always (Drop tagId itemData.itemId After), onEnter = Just (DragEnter tagId itemData.itemId After), onLeave = Just (DragLeave tagId itemData.itemId After) })
        [ div [ class "dropzone-internal" ] [] ]


isDnD itemData =
    case itemData.isDnD of
        True ->
            " dnd-active"

        False ->
            ""


isActive dropTargetPosition itemData =
    if itemData.isDnD == False then
        case itemData.dndOnOver of
            Just position ->
                case positionIsEqual position dropTargetPosition of
                    True ->
                        " active"

                    False ->
                        " dnd-active"

            Nothing ->
                ""

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
        , div [ class "item-internal-wrapper" ]
            [ div [ class "item-internal" ] [ img [ src itemData.src, alt itemData.fileName ] [] ]
            ]
        , div [ class "item-dropzone-right" ] []
        ]


debugLog s x =
    let
        _ =
            Debug.log s x
    in
    x
