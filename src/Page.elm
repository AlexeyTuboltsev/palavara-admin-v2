module Page exposing (..)

import AppData exposing (DropTargetPosition, ItemId, SectionId, TagData, TagId)


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
    , dnd: Maybe (TagId, ItemId)
    , dragOver: Maybe (TagId, ItemId, DropTargetPosition)
    }

type alias ItemViewData =
    { itemId : ItemId
    , fileName : String
    , urlString : String
    , src: String
    , isLoading: Bool
    }

type alias TagViewData =
    { label : String
    , tagId : TagId
    , items : List ItemViewData
    }