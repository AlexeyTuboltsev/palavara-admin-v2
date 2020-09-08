module Message exposing (..)

import AppData exposing (AppData, DropTargetPosition, ItemData, ItemId, SectionId, TagId)
import Browser
import Browser.Dom exposing (Viewport)
import File exposing (File)
import Html.Events.Extra.Drag as Drag exposing (Event)
import Http
import Json.Decode exposing (Value)
import Url exposing (Url)

type Msg
    = NoOp
    | GetViewport
    | SetViewport Viewport
    | SetData (Result Http.Error AppData)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GeneratePageData
    | SelectSection SectionId
    | Drop TagId ItemId DropTargetPosition
    | DragStart TagId ItemId Drag.EffectAllowed Value
    | DragOver Drag.DropEffect Value
    | DragEnter TagId ItemId DropTargetPosition Event
    | DragLeave TagId ItemId DropTargetPosition Event
    | DragEnd TagId ItemId Event
    | MoveItemLeft TagId ItemId
    | MoveItemRight TagId ItemId
    | EditItem ItemId
    | CancelEditItem
    | StartSaveingItem ItemId
    | HandleItemSaveResult (Result Http.Error String)
    | DeleteItem TagId ItemId
    | AskToDeleteItem TagId ItemId
    | ConfirmDeleteItemFromTag TagId ItemId
    | ConfirmGlobalDeleteItem ItemId
    | CancelDeleteItem
    | DeleteImage
    | AddImage (String, String)
    | ReadImage File
    | SelectImage