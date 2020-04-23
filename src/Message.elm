module Message exposing (..)

import AppData exposing (AppData, ItemId, SectionId, TagId)
import Browser
import Browser.Dom exposing (Viewport)
import Html.Events.Extra.Drag as Drag
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
    | Drop ItemId TagId
    | DragStart ItemId Drag.EffectAllowed Value
    | DragOver Drag.DropEffect Value
    | DragEnd