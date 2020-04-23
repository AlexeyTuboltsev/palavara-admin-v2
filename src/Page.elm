module Page exposing (..)

import AppData exposing (ItemData, SectionId, TagData)


type Page
    = SectionPage SectionPageData
    | InitialPage InitialPageData


type alias InitialPageData =
    { sections : List ( SectionId, String )
    }


type alias SectionPageData =
    { sections : List ( SectionId, String )
    , activeSectionId : SectionId
    , tags : List TagData
    , items : List ItemData
    }
