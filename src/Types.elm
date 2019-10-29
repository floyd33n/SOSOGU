module Types exposing (..)

import Bootstrap.Modal as BModal exposing (Visibility)
import Dict exposing (..)
import File exposing (..)
import Time exposing (..)


type Msg
    = ChangeCampusColor Point CssColor
    | InputColorValue String
    | AddColorToSubPalette CssColor
    | SetMainPalette Serial
    | DeleteSubPalette Serial
    | InputCampusWidth String
    | InputCampusHeight String
    | CreateCampus
    | CloseCreateCampusModalWindow
    | InputBorderColor String
    | SelectBorderStyle String
    | InputPixelWidth String
    | InputPixelHeight String
    | InputCampusPosition CampusPosition
    | InputSettingAndPalettePosition Panel Position
    | ApplySetting
    | Undo Point
    | GetImageUrl String
    | OpenSettingPanel
    | CloseSettingPanel
    | DLSavedata
    | UpSavedata
    | LoadSavedata File
    | DecodeSavedata String
    | ApplySavedata Model
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ShowSaveModalWindow
    | CloseSaveModalWindow
    | DLImage
    | NewProject
    | ShowSaveEditingCampusModalWindow YN
    | CloseSaveEditingCampusModalWindow
    | InputProjectName String


type alias Model =
    { campus : Campus
    , colorValue : CssColor
    , subPalette : SubPalette
    , mainPalette : CssColor
    , campusSize : CampusSize
    , didCreateCampus : Bool
    , setting : Setting
    , borderColorValue : CssColor
    , history : History
    , campusImageUrl : String
    , settingPanelStatus : PanelStatus
    , loadedSavedata : Savedata
    , timeGetter : TimeGetter
    , temp : Temp
    , modalStatus : ModalWindow
    , projectName : String
    }


type alias ModalWindow =
    { openingModalWindow : BModal.Visibility
    , saveModalWindow : BModal.Visibility
    , saveEditingCampusModalWindow : BModal.Visibility
    , saveAndNewCampusModalWindow : BModal.Visibility
    }


type alias Temp =
    { campusSize : TempCampusSize
    , setting : Setting
    }


type alias CssColor =
    String


type alias MainPaletteColor =
    String


type alias Campus =
    Dict Point CssColor


type alias Point =
    ( Int, Int )


type alias TempCampusSize =
    { width : String
    , height : String
    }


type alias CampusSize =
    { width : Int
    , height : Int
    }


type alias PixelSize =
    { width : String
    , height : String
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
    , pixelSize : PixelSize
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


type alias Savedata =
    String


type alias TimeGetter =
    { zone : Time.Zone
    , time : Time.Posix
    }


type YN
    = Yes
    | No
