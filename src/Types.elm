module Types exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
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
    | StartSavingItem ItemId
    | HandleSaveItemResult (Result Http.Error SaveImageResponse)
    | DeleteItem TagId ItemId
    | AskToDeleteItem TagId ItemId
    | ConfirmDeleteItemFromTag TagId ItemId
    | ConfirmGlobalDeleteItem ItemId
    | CancelDeleteItem
    | DeleteImage
    | AddImage (String, String)
    | ReadImage File
    | SelectImage
    | ChainUpdates Model (List Msg)



type View
    = Initial
    | Section SectionId


type Modal
    = ModalClosed
    | ConfirmDeleteItem TagId ItemId


type alias ItemEditorData =
    { itemId : ItemId
    , fileName : Maybe String
    , src: Maybe String
    , urlString : Maybe String
    , usedIn : List OrderListId
    }


type ItemEditor
    = ItemEditorClosed
    | ItemEditorOpen ItemEditorData
    | ItemEditorLoading ItemEditorData


type alias UIData =
    { view : View
    , imageUrl : String
    , dnd : Maybe ( TagId, ItemId )
    , dragOver : Maybe ( TagId, ItemId, DropTargetPosition )
    , modal : Modal
    , itemEditor : ItemEditor
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
    , isDnD : Bool
    , dndOnOver : Maybe DropTargetPosition
    }


type alias TagViewData =
    { label : String
    , tagId : TagId
    , items : List ItemViewData
    }

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
    , tempData : Maybe (List SectionData)
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

type alias SectionId =
    String


type alias TagId =
    String


type alias ItemId =
    String


type alias TagData =
    { label : String
    , tagId : TagId
    , items : List ItemData
    }


type alias ItemData =
    { itemId : ItemId
    , fileName : String
    , urlString : String
    }


type SectionData
    = GalleryWithTagsSectionType GalleryWithTagsSectionData
    | GallerySectionType GallerySectionData
    | InfoSectionType InfoSectionData


type alias GalleryWithTagsSectionData =
    { label : String
    , sectionId : SectionId
    , items : List ItemData
    , tags : List TagData
    }


type alias InfoSectionData =
    { label : String
    , sectionId : SectionId
    , text : String
    , imageId : String
    }


type alias GallerySectionData =
    { label : String
    , sectionId : SectionId
    }


type alias AppData =
    List SectionData


type alias OrderListId =
    String


type alias ItemDataNext =
    { itemId : ItemId
    , fileName : String
    , urlString : String
    , usedIn : List OrderListId
    }


type alias TagDataNext =
    { tagId : TagId
    , label : String
    , itemOrderId : OrderListId
    , usedIn : List OrderListId
    }


type alias GalleryWithTagsSectionDataNext =
    { sectionId : SectionId
    , label : String
    , tagOrderId : OrderListId
    , itemOrderId : OrderListId
    , usedIn : List OrderListId
    }


type alias GallerySectionDataNext =
    { sectionId : String
    , label : String
    }


type alias InfoSectionDataNext =
    { sectionId : String
    , label : String
    , text : String
    , imageId : String
    }


type SectionDataNext
    = GalleryWithTagsSectionNext GalleryWithTagsSectionDataNext
    | GallerySectionNext GallerySectionDataNext
    | InfoSectionNext InfoSectionDataNext


type alias AppDataNext =
    { sections : Dict SectionId SectionDataNext
    , orderLists : Dict OrderListId (List String)
    , tags : Dict String TagDataNext
    , items : Dict ItemId ItemDataNext
    }


type DropTargetPosition
    = Before
    | After


type alias SaveImageResponse =
    {itemId:String,fileName:String}