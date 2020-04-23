module Main exposing (..)

import AppData exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewport, getViewportOf)
import Browser.Events exposing (onResize)
import Browser.Navigation as Navigation exposing (Key, load)
import Html exposing (Html, a, br, div, img, span, text)
import Html.Attributes exposing (class, href, id, property, src, style, alt)
import Html.Events.Extra exposing (onClickPreventDefault)
import Html.Events.Extra.Drag exposing (DropEffect(..), onDropTarget, onSourceDrag)
import Http exposing (expectJson, get)
import Icons
import List.Extra exposing (find)
import Message exposing (Msg(..))
import Page exposing (Page(..))
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

                        newModel =
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
                                                Just ( sd.tags, sd.items )

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.map (\( tags, items ) -> SectionPage { activeSectionId = sectionId, sections = sectionData, tags = tags, items = items })
                                |> Maybe.map (\page -> { readyModelData | page = page })
                    in
                    case newModel of
                        Just nm ->
                            ( ReadyModel nm, Cmd.none )

                        Nothing ->
                            ( ReadyModel readyModelData, Cmd.none )

                UrlChanged url ->
                    ( ReadyModel readyModelData, Cmd.none )

                _ ->
                    ( model, Cmd.none )


allFieldsPresent : Model -> Maybe ReadyModelData
allFieldsPresent newModel =
    case newModel of
        InitModel data ->
            Maybe.map5
                (\appData urlString key url page ->
                    { data = appData, apiUrl = urlString, key = key, page = page }
                )
                data.data
                (Just data.dataUrl)
                (Just data.key)
                (Just data.url)
                (data.data
                    |> Maybe.map (\d -> getSectionData d (\s -> ( s.sectionId, s.label )))
                    |> Maybe.map (\d -> Page.InitialPageData d)
                    |> Maybe.map (\initPageData -> InitialPage initPageData)
                )

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
                [ div [ class "menu" ] (menu sections)
                ]
            ]

        SectionPage { sections, activeSectionId, tags, items } ->
            [ div [ class "layout" ]
                [ div [ class "menu" ] (menu sections)
                , div [ class "section" ]
                    [ div [ class "sectionTitle" ] [ text activeSectionId ]
                    , div [ class "items" ] (itemList items)
                    , div [ class "tags" ] (tagList tags)
                    ]
                ]
            ]


menu : List ( SectionId, String ) -> List (Html Msg)
menu sections =
    List.map
        (\( sectionId, label ) ->
            div [ class "menu-section" ]
                [ div [ onClickPreventDefault (SelectSection sectionId) ] [ text label ]
                ]
        )
        sections


tagList : List TagData -> List (Html Msg)
tagList tagData =
    List.map
        (\tag ->
            div [ class "tag" ]
                [ text tag.label
                , div [ class "items" ] (itemList tag.items)
                ]
        )
        tagData


itemList items =
    List.indexedMap
        (\i item ->
            div ( class "item" ::(onSourceDrag {effectAllowed = {move = True, copy = False, link = False}, onStart = DragStart item.itemId, onEnd = always DragEnd, onDrag = Nothing}))
                [
                 div (class "item-dropzone" :: (onDropTarget {dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop item.itemId "bla"), onEnter = Nothing, onLeave = Nothing})) []
                 , div [class "item-internal"] [ img [ src item.fileName, alt item.fileName ] []]
                 , div  (class "item-dropzone" :: (onDropTarget {dropEffect = MoveOnDrop, onOver = DragOver, onDrop = always (Drop item.itemId "bla"), onEnter = Nothing, onLeave = Nothing})) []
                ]
        )
        items



--
--debugLog s x =
--    let
--        _ =
--            Debug.log s x
--    in
--    x
