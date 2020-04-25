module Message exposing (..)

import AppData exposing (AppData, DropTargetPosition, ItemId, SectionId, TagId)
import Browser
import Browser.Dom exposing (Viewport)
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
    | SelectSection SectionId
    | Drop ItemId DropTargetPosition
    | DragStart ItemId Drag.EffectAllowed Value
    | DragOver Drag.DropEffect Value
    | DragEnter TagId ItemId DropTargetPosition Event
    | DragLeave TagId ItemId DropTargetPosition Event
    | DragEnd TagId ItemId Event