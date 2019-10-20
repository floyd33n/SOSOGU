module Types exposing (..)

import Bootstrap.Modal as BModal exposing (Visibility)
import Dict exposing (..)
import Savedata exposing (Savedata)
import Time exposing (..)


type alias Model =
    { campus : Campus
    , colorValue : CssColor
    , subPalette : SubPalette
    , mainPalette : CssColor
    , campusSize : CampusSize
    , tempCampusSize : TempCampusSize
    , didCreateCampus : Bool
    , modalVisibility : BModal.Visibility
    , openingModalWindow : BModal.Visibility
    , setting : Setting
    , tempSetting : Setting
    , borderColorValue : CssColor
    , toolsSetting : ToolsSetting
    , history : History
    , campusImageUrl : String
    , settingPanelStatus : PanelStatus
    , loadedSavedata : Savedata
    , timeGetter : TimeGetter
    , saveModalWindow : BModal.Visibility
    , saveEditingCampusModalWindow : BModal.Visibility
    , saveAndNewCampusModalWindow : BModal.Visibility
    }


type alias CssColor =
    String


type alias Campus =
    Dict Point CssColor


type alias Point =
    ( Int, Int )


type alias Points =
    ( Int, Int )


type alias TempCampusSize =
    { width : String
    , height : String
    }


type alias CampusSize =
    { width : Int
    , height : Int
    }


type alias Serial =
    Int


type alias SubPalette =
    Dict Serial CssColor


type alias History =
    Dict Serial ( Point, CssColor )


type alias Setting =
    { borderColor : CssColor
    , borderStyle : BorderStyle
    , width : String
    , height : String
    , panelPosition : PanelPosition
    }


type alias BorderStyle =
    String


type alias PanelPosition =
    { settingPanel : Position
    , palettePanel : Position
    , campusPanel : CampusPosition
    }


type CampusPosition
    = TopCenter
    | TopRight
    | TopLeft


type Position
    = Right
    | Left


type Panel
    = SettingPanel
    | PalettePanel


type PanelStatus
    = Open
    | Close


type alias TimeGetter =
    { zone : Time.Zone
    , time : Time.Posix
    }


type YN
    = Yes
    | No


type alias ToolsSetting =
    { isDisplayDlButton : Bool
    }
