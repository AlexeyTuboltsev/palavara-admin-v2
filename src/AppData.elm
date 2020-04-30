module AppData exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE


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



-- JSON --


appDataDecoder : JD.Decoder (List SectionData)
appDataDecoder =
    JD.field "sections" (JD.list sectionDataDecoder)


itemDataDecoder : JD.Decoder ItemData
itemDataDecoder =
    JD.map3 ItemData
        (JD.field "itemId" JD.string)
        (JD.field "fileName" JD.string)
        (JD.field "urlString" JD.string)


tagDataDecoder : JD.Decoder TagData
tagDataDecoder =
    JD.map3 TagData
        (JD.field "label" JD.string)
        (JD.field "tagId" JD.string)
        (JD.field "items" (JD.list itemDataDecoder))


itemsDecoder =
    JD.list itemDataDecoder


sectionDataDecoder : JD.Decoder SectionData
sectionDataDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\sectionType ->
                case sectionType of
                    "galleryWithTags" ->
                        JD.map4 GalleryWithTagsSectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            (JD.field "items" itemsDecoder)
                            (JD.field "tags" (JD.list tagDataDecoder))
                            |> JD.map GalleryWithTagsSectionType

                    "gallery" ->
                        JD.map2 GallerySectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            |> JD.map GallerySectionType

                    "info" ->
                        JD.map4 InfoSectionData
                            (JD.field "label" JD.string)
                            (JD.field "sectionId" JD.string)
                            (JD.field "text" JD.string)
                            (JD.field "imageId" JD.string)
                            |> JD.map InfoSectionType

                    _ ->
                        JD.fail "no luck today"
            )

----- Encoders ---------------
encodeAppData appData =
    JE.list sectionEncoder appData


sectionEncoder section =
    case section of
        GalleryWithTagsSectionType data ->
            JE.object
                [ ( "label", JE.string data.label )
                , ( "sectionId", JE.string data.sectionId )
                , ( "items", JE.list itemEncoder data.items )
                , ( "tags", JE.list tagEncoder data.tags )
                ]

        _ ->
            JE.object []



--todo


tagEncoder tag =
    JE.object
        [ ( "label", JE.string tag.label )
        , ( "tagId", JE.string tag.tagId )
        , ( "items", JE.list itemEncoder tag.items )
        ]


itemEncoder item =
    JE.object
        [ ( "itemId", JE.string item.itemId )
        , ( "fileName", JE.string item.fileName )
        , ( "urlString", JE.string item.urlString )
        ]
