port module Main exposing (..)

--import Debug exposing (..)

import Array exposing (..)
import Base64 exposing (..)
import Bootstrap.Alert as BAlert exposing (..)
import Bootstrap.Button as BBtn exposing (..)
import Bootstrap.CDN as BCDN exposing (..)
import Bootstrap.Form.Input as BInput exposing (..)
import Bootstrap.Form.Radio as BRadio exposing (..)
import Bootstrap.Form.Select as BSelect exposing (..)
import Bootstrap.General.HAlign as BGHAlign exposing (..)
import Bootstrap.Grid as BGrid exposing (..)
import Bootstrap.Grid.Col as BCol exposing (..)
import Bootstrap.Grid.Row as BRow exposing (..)
import Bootstrap.Modal as BModal exposing (..)
import Bootstrap.Text as BText exposing (..)
import Bootstrap.Utilities.Border as BUtilsBorder exposing (..)
import Bootstrap.Utilities.Flex as BUtilsFlex exposing (..)
import Bootstrap.Utilities.Size as BUtilsSize exposing (..)
import Bootstrap.Utilities.Spacing as BUtilsSpacing exposing (..)
import Browser
import Bytes exposing (..)
import Bytes.Decode as BD exposing (..)
import Bytes.Encode as BE exposing (..)
import Dict exposing (..)
import Dict.Extra as DictEx exposing (..)
import Element as E exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Font as Font exposing (..)
import Element.Input as Input exposing (..)
import Element.Region as Region exposing (..)
import File exposing (..)
import File.Download as FileDL exposing (..)
import File.Select as FileSel exposing (..)
import Html as H exposing (..)
import Html.Attributes as HAttrs exposing (..)
import Html.Events as HEvents exposing (..)
import Html.Lazy as HLazy exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import List.Extra as ListEx exposing (..)
import Parser as P exposing (..)
import Platform.Sub exposing (..)
import Regex exposing (..)
import Result.Extra as ResultEX exposing (..)
import Svg exposing (..)
import Svg.Attributes as SAttrs exposing (..)
import Svg.Events as SEvents exposing (..)
import Task exposing (..)
import Time exposing (..)


css path =
    H.node "link" [ rel "stylesheet", href path ] []



--MODEL--


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


type alias Campus =
    Dict Point CssColor


type alias Point =
    ( Int, Int )


type alias Points =
    ( Int, Int )


type alias CssColor =
    String


initMainPalette : CssColor
initMainPalette =
    "white"


initColorValue : CssColor
initColorValue =
    "white"


initBorderColorValue : CssColor
initBorderColorValue =
    "black"


initCampus : Campus
initCampus =
    Dict.empty


type alias TempCampusSize =
    { width : String
    , height : String
    }


type alias CampusSize =
    { width : Int
    , height : Int
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


type alias Serial =
    Int


type alias SubPalette =
    Dict Serial CssColor


initSubPalette : SubPalette
initSubPalette =
    Dict.fromList [ ( 0, "white" ) ]


type alias History =
    Dict Serial ( Point, CssColor )


initHistory : History
initHistory =
    {-
       Dict.fromList <|
           ListEx.lift2 Tuple.pair (List.range 0 0)
               ListEx.lift2 Tuple.pair (ListEx.lift2 Tuple.pair (List.range 0 0) (List.range 0 0)) [ "white" ]
    -}
    --Dict.fromList [ ( 0, ( ( 0, 0 ), "white" ) ) ]
    Dict.empty


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


initPanelPosition : PanelPosition
initPanelPosition =
    { settingPanel = Left
    , palettePanel = Right
    , campusPanel = TopCenter
    }


initSetting : Setting
initSetting =
    { borderColor = "black"
    , borderStyle = "solid"
    , width = "20"
    , height = "20"
    , panelPosition = initPanelPosition
    }


type alias ToolsSetting =
    { isDisplayDlButton : Bool
    }


initToolsSetting : ToolsSetting
initToolsSetting =
    { isDisplayDlButton = False
    }


type PanelStatus
    = Open
    | Close


type alias Savedata =
    String


type alias TimeGetter =
    { zone : Time.Zone
    , time : Time.Posix
    }


initTimeGetter : TimeGetter
initTimeGetter =
    { zone = Time.utc
    , time = Time.millisToPosix 0
    }


type YN
    = Yes
    | No



--INIT--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { campus = initCampus
      , colorValue = initColorValue
      , subPalette = initSubPalette
      , mainPalette = initMainPalette
      , campusSize = CampusSize 0 0
      , tempCampusSize = TempCampusSize "" ""
      , didCreateCampus = False
      , modalVisibility = BModal.hidden
      , openingModalWindow = BModal.shown
      , setting = initSetting
      , tempSetting = initSetting
      , borderColorValue = initBorderColorValue
      , toolsSetting = initToolsSetting
      , history = initHistory
      , campusImageUrl = ""
      , settingPanelStatus = Close
      , loadedSavedata = ""
      , timeGetter = initTimeGetter
      , saveModalWindow = BModal.hidden
      , saveEditingCampusModalWindow = BModal.hidden
      , saveAndNewCampusModalWindow = BModal.hidden
      }
    , Task.perform AdjustTimeZone Time.here
    )



--UPDATE--


type Msg
    = ChangeColor Point CssColor
    | ColorValue String
    | AddColorToSubPalette CssColor
    | SetMainPalette Serial
    | DeleteSubPalette Serial
    | SetCampusWidth String
    | SetCampusHeight String
    | CreateCampus
    | ShowModal
    | CloseCreateCampusWindow
    | BorderColorValue String
    | Change String
    | ChangePixelSize String String
    | SetPixelWidth String
    | SetPixelHeight String
    | SetCampusPosition CampusPosition
    | ChangePanelPosition Panel Position
    | ApplySetting
    | Undo Point
    | GetImageUrl String
    | OpenSettingPanel
    | CloseSettingPanel
    | DLSavedata
    | UpSavedata
    | LoadSavedata File
    | ToStringSavedata String
    | ApplySavedata Model
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ShowSaveWindow
    | CloseSaveWindow
    | DLImage
    | NewProject
    | SaveEditingCampusModalWindow YN
    | CloseSaveEditingCampusModalWindow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setting_ =
            model.setting

        tempSetting_ =
            model.tempSetting

        panelPosition_ =
            model.tempSetting.panelPosition

        timeGetter_ =
            model.timeGetter
    in
    case msg of
        ChangeColor ( x, y ) color ->
            ( { model
                | campus =
                    Dict.update
                        ( x, y )
                        (Maybe.map (\n -> color))
                        model.campus
                , history =
                    if Dict.size model.history <= 100 then
                        Dict.insert
                            (Dict.size model.history)
                            ( ( x, y )
                            , getCampusColor model.campus ( x, y )
                            )
                            model.history

                    else
                        let
                            tempHistory =
                                model.history
                                    |> Dict.toList
                                    |> List.drop 1
                                    |> Dict.fromList
                                    |> DictEx.mapKeys (\n -> n - 1)
                        in
                        Dict.insert
                            (Dict.size tempHistory)
                            ( ( x, y )
                            , getCampusColor model.campus ( x, y )
                            )
                            tempHistory
              }
            , Cmd.none
            )

        ColorValue value ->
            ( { model
                | colorValue =
                    if String.isEmpty model.colorValue then
                        value

                    else
                        String.toLower value
              }
            , Cmd.none
            )

        AddColorToSubPalette color ->
            ( { model
                | subPalette = addColorToSubPalette model.subPalette color
                , mainPalette = model.colorValue
              }
            , Cmd.none
            )

        SetMainPalette n ->
            ( { model
                | mainPalette = getSubPaletteColor model.subPalette n
              }
            , Cmd.none
            )

        DeleteSubPalette n ->
            ( { model
                | subPalette =
                    Dict.union
                        (model.subPalette
                            |> Dict.toList
                            |> List.take (n - 1)
                            |> Dict.fromList
                        )
                        (model.subPalette
                            |> Dict.toList
                            |> List.drop n
                            |> Dict.fromList
                            |> DictEx.mapKeys (\m -> m - 1)
                        )
                , mainPalette = "white"
              }
            , Cmd.none
            )

        SetCampusWidth width ->
            ( { model
                | tempCampusSize =
                    { width = width
                    , height = model.tempCampusSize.height
                    }
              }
            , Cmd.none
            )

        SetCampusHeight height ->
            ( { model
                | tempCampusSize =
                    { height = height
                    , width = model.tempCampusSize.width
                    }
              }
            , Cmd.none
            )

        CreateCampus ->
            let
                createCampusList : Points -> List ( Point, CssColor )
                createCampusList ( width_, height_ ) =
                    ListEx.lift2
                        Tuple.pair
                        (ListEx.lift2
                            Tuple.pair
                            (List.range 0 width_)
                            (List.range 0 height_)
                        )
                        [ "white" ]

                convertTemp : String -> Int
                convertTemp str =
                    Maybe.withDefault 0 (String.toInt str)
            in
            ( { model
                | campus =
                    Dict.fromList <|
                        createCampusList
                            ( convertTemp model.tempCampusSize.width - 1
                            , convertTemp model.tempCampusSize.height - 1
                            )
                , campusSize =
                    { width = convertTemp model.tempCampusSize.width
                    , height = convertTemp model.tempCampusSize.height
                    }
                , openingModalWindow = BModal.hidden
                , didCreateCampus = True
              }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = BModal.shown }
            , Cmd.none
            )

        CloseCreateCampusWindow ->
            ( { model | openingModalWindow = BModal.hidden }
            , Cmd.none
            )

        BorderColorValue value ->
            ( { model
                | tempSetting =
                    { borderColor =
                        if String.isEmpty value then
                            model.setting.borderColor

                        else
                            String.toLower value
                    , borderStyle = model.tempSetting.borderStyle
                    , width = model.tempSetting.width
                    , height = model.tempSetting.height
                    , panelPosition = model.tempSetting.panelPosition
                    }
              }
            , Cmd.none
            )

        Change str ->
            ( { model
                | tempSetting =
                    { tempSetting_
                        | borderStyle = str
                    }
              }
            , Cmd.none
            )

        ChangePixelSize width_ height_ ->
            if Maybe.withDefault 0 (String.toInt width_) * Maybe.withDefault 0 (String.toInt height_) > 0 then
                ( { model
                    | tempSetting =
                        { tempSetting_
                            | width = width_
                            , height = height_
                        }
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SetPixelWidth tempWidth_ ->
            ( { model
                | tempSetting =
                    { tempSetting_
                        | width =
                            if String.isEmpty tempWidth_ then
                                model.setting.width

                            else
                                tempWidth_
                    }
              }
            , Cmd.none
            )

        SetPixelHeight tempHeight_ ->
            ( { model
                | tempSetting =
                    { tempSetting_
                        | height =
                            if String.isEmpty tempHeight_ then
                                model.setting.height

                            else
                                tempHeight_
                    }
              }
            , Cmd.none
            )

        SetCampusPosition position_ ->
            ( { model
                | tempSetting =
                    { tempSetting_
                        | panelPosition =
                            { panelPosition_
                                | campusPanel = position_
                            }
                    }
              }
            , Cmd.none
            )

        GetImageUrl url ->
            ( { model | campusImageUrl = url }
            , Cmd.none
            )

        ChangePanelPosition panel_ position_ ->
            case panel_ of
                SettingPanel ->
                    case position_ of
                        Right ->
                            ( { model
                                | tempSetting =
                                    { tempSetting_
                                        | panelPosition =
                                            { panelPosition_
                                                | settingPanel = Right
                                            }
                                    }
                              }
                            , Cmd.none
                            )

                        Left ->
                            ( { model
                                | tempSetting =
                                    { tempSetting_
                                        | panelPosition =
                                            { panelPosition_
                                                | settingPanel = Left
                                            }
                                    }
                              }
                            , Cmd.none
                            )

                PalettePanel ->
                    case position_ of
                        Right ->
                            ( { model
                                | tempSetting =
                                    { tempSetting_
                                        | panelPosition =
                                            { panelPosition_
                                                | palettePanel = Right
                                            }
                                    }
                              }
                            , Cmd.none
                            )

                        Left ->
                            ( { model
                                | tempSetting =
                                    { tempSetting_
                                        | panelPosition =
                                            { panelPosition_
                                                | palettePanel = Left
                                            }
                                    }
                              }
                            , Cmd.none
                            )

        ApplySetting ->
            if isCorrectSetting model.tempSetting then
                ( { model | setting = model.tempSetting }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Undo ( y, x ) ->
            let
                getHistoryColor : Point -> String
                getHistoryColor ( x_, y_ ) =
                    Tuple.second <|
                        Maybe.withDefault ( ( 0, 0 ), "white" ) <|
                            Dict.get (Dict.size model.history - 1) model.history
            in
            ( { model
                | campus =
                    Dict.update
                        ( x, y )
                        (Maybe.map (\n -> getHistoryColor ( x, y )))
                        model.campus
                , history = Dict.remove (Dict.size model.history - 1) model.history
              }
            , Cmd.none
            )

        OpenSettingPanel ->
            ( { model
                | settingPanelStatus = Open
              }
            , Cmd.none
            )

        CloseSettingPanel ->
            ( { model
                | settingPanelStatus = Close
              }
            , Cmd.none
            )

        DLSavedata ->
            ( model
            , dlSavedata model
            )

        UpSavedata ->
            ( model
            , upSavedata
            )

        LoadSavedata savedata ->
            ( model
            , toStringSaveData savedata
            )

        ToStringSavedata savedata ->
            ( { model
                | loadedSavedata = decodeSavedata savedata
              }
            , applySavedata model
            )

        ApplySavedata model_ ->
            let
                savedata =
                    model.loadedSavedata
            in
            if isSOSOGUSavedata model.loadedSavedata then
                ( { model
                    | setting = settingFromSavedata savedata
                    , campus = campusFromSavedata savedata
                    , mainPalette = mainPaletteFromSavedata savedata
                    , campusSize = campusSizeFromSavedata savedata
                    , didCreateCampus = didCreateCampusFromSavedata savedata
                    , toolsSetting = toolsSettingFromSavedata savedata
                    , subPalette = subPaletteFromSavedata savedata
                    , history = historyFromSavedata savedata
                    , openingModalWindow = BModal.hidden
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Tick newTime ->
            ( { model | timeGetter = { timeGetter_ | time = newTime } }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | timeGetter = { timeGetter_ | zone = newZone } }, Cmd.none )

        ShowSaveWindow ->
            ( { model | saveModalWindow = BModal.shown }
            , generateCampusImage ()
            )

        CloseSaveWindow ->
            ( { model
                | saveModalWindow = BModal.hidden
                , saveAndNewCampusModalWindow = BModal.hidden
              }
            , Cmd.none
            )

        DLImage ->
            ( model
            , dlImage model.campusImageUrl
            )

        NewProject ->
            ( if model.didCreateCampus then
                { model
                    | saveEditingCampusModalWindow = BModal.shown
                }

              else
                { model
                    | openingModalWindow = BModal.shown
                }
            , Cmd.none
            )

        SaveEditingCampusModalWindow yn ->
            case yn of
                Yes ->
                    ( { model
                        | saveEditingCampusModalWindow = BModal.hidden
                        , saveAndNewCampusModalWindow = BModal.shown
                      }
                    , Cmd.none
                    )

                No ->
                    ( { model
                        | saveEditingCampusModalWindow = BModal.hidden
                        , saveAndNewCampusModalWindow = BModal.hidden
                        , openingModalWindow = BModal.shown
                      }
                    , Cmd.none
                    )

        CloseSaveEditingCampusModalWindow ->
            ( { model
                | saveEditingCampusModalWindow = BModal.hidden
              }
            , Cmd.none
            )


dlImage : String -> Cmd Msg
dlImage url =
    let
        base64ToBytes =
            case
                Base64.toBytes <|
                    String.dropLeft 22 url
            of
                Nothing ->
                    Just dummyBytes

                Just bytes ->
                    Just bytes

        dummyBytes =
            BE.encode (BE.string "")

        imageBytes =
            Maybe.withDefault dummyBytes base64ToBytes
    in
    FileDL.bytes "campus.png" "image/png" imageBytes


isSOSOGUSavedata : Savedata -> Bool
isSOSOGUSavedata savedata =
    case
        JD.decodeString (JD.field "general" (JD.field "isSOSOGUSavedata" JD.bool)) savedata
    of
        Ok bool ->
            bool

        Err _ ->
            False


toolsSettingFromSavedata : Savedata -> ToolsSetting
toolsSettingFromSavedata savedata =
    { isDisplayDlButton =
        case
            JD.decodeString (JD.field "toolsSetting" (JD.field "isDisplayDlButton" JD.bool)) savedata
        of
            Ok bool ->
                bool

            Err _ ->
                False
    }


didCreateCampusFromSavedata : Savedata -> Bool
didCreateCampusFromSavedata savedata =
    case
        JD.decodeString (JD.field "didCreateCampus" JD.bool) savedata
    of
        Ok bool ->
            bool

        Err _ ->
            False


campusSizeFromSavedata : Savedata -> CampusSize
campusSizeFromSavedata savedata =
    { width =
        case
            JD.decodeString (JD.field "campusSize" (JD.field "width" JD.int)) savedata
        of
            Ok width ->
                width

            Err _ ->
                8
    , height =
        case
            JD.decodeString (JD.field "campusSize" (JD.field "height" JD.int)) savedata
        of
            Ok height ->
                height

            Err _ ->
                8
    }


mainPaletteFromSavedata : Savedata -> CssColor
mainPaletteFromSavedata savedata =
    case
        JD.decodeString (JD.field "mainPalette" JD.string) savedata
    of
        Ok color ->
            color

        Err _ ->
            "white"


settingFromSavedata : Savedata -> Setting
settingFromSavedata savedata =
    let
        decodeBorderColor : CssColor
        decodeBorderColor =
            case
                JD.decodeString (JD.field "setting" (JD.field "borderColor" JD.string)) savedata
            of
                Ok color ->
                    color

                Err _ ->
                    "black"

        decodeBorderStyle : String
        decodeBorderStyle =
            case
                JD.decodeString (JD.field "setting" (JD.field "borderStyle" JD.string)) savedata
            of
                Ok style ->
                    style

                Err _ ->
                    "solid"

        decodeWidth : String
        decodeWidth =
            case
                JD.decodeString (JD.field "setting" (JD.field "width" JD.string)) savedata
            of
                Ok width ->
                    width

                Err _ ->
                    "20"

        decodeHeight : String
        decodeHeight =
            case
                JD.decodeString (JD.field "setting" (JD.field "height" JD.string)) savedata
            of
                Ok height ->
                    height

                Err _ ->
                    "20"

        decodePanelPosition : PanelPosition
        decodePanelPosition =
            { settingPanel =
                case
                    JD.decodeString (JD.field "setting" (JD.field "panelPosition" (JD.field "settingPanel" JD.string))) savedata
                of
                    Ok settingPanel ->
                        case settingPanel of
                            "Right" ->
                                Right

                            "Left" ->
                                Left

                            _ ->
                                Left

                    Err _ ->
                        Left
            , palettePanel =
                case
                    JD.decodeString (JD.field "setting" (JD.field "panelPosition" (JD.field "palettePanel" JD.string))) savedata
                of
                    Ok palettePanel ->
                        case palettePanel of
                            "Right" ->
                                Right

                            "Left" ->
                                Left

                            _ ->
                                Right

                    Err _ ->
                        Right
            , campusPanel =
                case
                    JD.decodeString (JD.field "setting" (JD.field "panelPositon" (JD.field "campusPanel" JD.string))) savedata
                of
                    Ok campusPanel ->
                        case campusPanel of
                            "TopCenter" ->
                                TopCenter

                            "TopRight" ->
                                TopRight

                            "TopLeft" ->
                                TopLeft

                            _ ->
                                TopCenter

                    Err _ ->
                        TopCenter
            }
    in
    { borderColor = decodeBorderColor
    , borderStyle = decodeBorderStyle
    , width = decodeWidth
    , height = decodeHeight
    , panelPosition = decodePanelPosition
    }


historyFromSavedata : Savedata -> History
historyFromSavedata savedata =
    let
        decodeHistory : List String
        decodeHistory =
            case
                JD.decodeString (JD.field "history" (JD.list JD.string)) savedata
            of
                Ok history ->
                    history

                Err _ ->
                    []

        getSerial n historyData =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (ListEx.getAt 0 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData))))))

        getPoint n historyData =
            let
                getX =
                    Maybe.withDefault 0 (String.toInt (String.dropLeft 1 (Maybe.withDefault "(0" (ListEx.getAt 1 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData)))))))

                getY =
                    Maybe.withDefault 0 (String.toInt (String.dropRight 1 (Maybe.withDefault "0)" (ListEx.getAt 2 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData)))))))
            in
            ( getX, getY )

        getColor n historyData =
            Maybe.withDefault "white" (ListEx.getAt 3 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData))))

        makeHistory n historyData =
            Tuple.pair (getSerial n historyData) (Tuple.pair (getPoint n historyData) (getColor n historyData))
    in
    Dict.fromList <|
        List.map (\n -> makeHistory n decodeHistory) <|
            List.range 0 (List.length decodeHistory - 1)


subPaletteFromSavedata : Savedata -> SubPalette
subPaletteFromSavedata savedata =
    let
        decodeSubPalette : List String
        decodeSubPalette =
            case
                JD.decodeString (JD.field "subPalette" (JD.list JD.string)) savedata
            of
                Ok subPalette ->
                    subPalette

                Err _ ->
                    []

        getSerial n subPaletteData =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (ListEx.getAt 0 (String.split "," (Maybe.withDefault "0,white" (ListEx.getAt n subPaletteData))))))

        getColor n subPaletteData =
            Maybe.withDefault "white" (ListEx.getAt 1 (String.split "," (Maybe.withDefault "0,white" (ListEx.getAt n subPaletteData))))

        makeSubPalette n subPaletteData =
            Tuple.pair (getSerial n subPaletteData) (getColor n subPaletteData)
    in
    Dict.fromList <|
        List.map (\n -> makeSubPalette n decodeSubPalette) <|
            List.range 0 (List.length decodeSubPalette - 1)


applySavedata : Model -> Cmd Msg
applySavedata model =
    Task.perform ApplySavedata (Task.succeed model)


campusFromSavedata : Savedata -> Campus
campusFromSavedata savedata =
    let
        decodeCampus : List String
        decodeCampus =
            case
                JD.decodeString (JD.field "campus" (JD.list JD.string)) savedata
            of
                Ok campus ->
                    campus

                Err _ ->
                    []

        getPoint : Int -> List String -> Point
        getPoint n campusData =
            let
                getX : Int
                getX =
                    Maybe.withDefault 0 (String.toInt (String.slice 1 2 (Maybe.withDefault "0" (ListEx.getAt n campusData))))

                getY : Int
                getY =
                    Maybe.withDefault 0 (String.toInt (String.slice 3 4 (Maybe.withDefault "0" (ListEx.getAt n campusData))))
            in
            ( getX, getY )

        getColor : Int -> List String -> CssColor
        getColor n campusData =
            Maybe.withDefault "black" (ListEx.getAt 2 (String.split "," (Maybe.withDefault "white" (ListEx.getAt n campusData))))

        makeCampusList : Int -> List String -> ( Point, CssColor )
        makeCampusList n campusData =
            Tuple.pair (getPoint n campusData) (getColor n campusData)
    in
    Dict.fromList <|
        List.map (\n -> makeCampusList n decodeCampus) <|
            List.range 0 (List.length decodeCampus - 1)


toStringSaveData : File -> Cmd Msg
toStringSaveData file =
    Task.perform ToStringSavedata (File.toString file)


dlSavedata : Model -> Cmd msg
dlSavedata model =
    let
        toStringYear =
            String.fromInt (Time.toYear model.timeGetter.zone model.timeGetter.time)

        toStringMonth =
            monthToString (Time.toMonth model.timeGetter.zone model.timeGetter.time)

        toStringDay =
            String.fromInt (Time.toDay model.timeGetter.zone model.timeGetter.time)

        toStringHour =
            let
                hour =
                    String.fromInt (Time.toHour model.timeGetter.zone model.timeGetter.time)
            in
            if String.length hour == 1 then
                "0" ++ hour

            else if String.length hour == 2 then
                hour

            else
                hour

        toStringMinute =
            String.fromInt (Time.toMinute model.timeGetter.zone model.timeGetter.time)

        toStringSecond =
            String.fromInt (Time.toSecond model.timeGetter.zone model.timeGetter.time)

        monthToString : Month -> String
        monthToString month =
            case month of
                Jan ->
                    "Jan"

                Feb ->
                    "Feb"

                Mar ->
                    "Mar"

                Apr ->
                    "Apr"

                May ->
                    "May"

                Jun ->
                    "Jun"

                Jul ->
                    "Jul"

                Aug ->
                    "Aug"

                Sep ->
                    "Sep"

                Oct ->
                    "Oct"

                Nov ->
                    "Nov"

                Dec ->
                    "Dev"
    in
    FileDL.string
        ("sosogu"
            ++ "-"
            ++ toStringHour
            ++ "-"
            ++ toStringMinute
            ++ "_"
            ++ toStringYear
            ++ "."
            ++ toStringMonth
            ++ "."
            ++ toStringDay
            ++ ".json"
        )
        ""
        (encodeSavedataWithBase64 model)


upSavedata : Cmd Msg
upSavedata =
    FileSel.file [ "" ] LoadSavedata


encodeSavedataWithBase64 model =
    Maybe.withDefault "fk" (Base64.fromBytes (BE.encode (BE.string (makeSavedata model))))


decodeSavedata savedata =
    let
        dummy =
            BE.encode (BE.string "")

        b64ToBytes =
            Maybe.withDefault dummy (Base64.toBytes savedata)
    in
    Maybe.withDefault "" (BD.decode (BD.string (Bytes.width b64ToBytes)) b64ToBytes)


makeSavedata : Model -> String
makeSavedata model =
    let
        campusData : List String
        campusData =
            List.concat <|
                List.map
                    (\x ->
                        List.map
                            (\y ->
                                "("
                                    ++ String.fromInt x
                                    ++ ","
                                    ++ String.fromInt y
                                    ++ "),"
                                    ++ getCampusColor model.campus ( x, y )
                            )
                        <|
                            List.range 0 (model.campusSize.width - 1)
                    )
                <|
                    List.range 0 (model.campusSize.height - 1)

        historyData : List String
        historyData =
            let
                historyValue key =
                    Maybe.withDefault ( ( 0, 0 ), "white" ) <|
                        Dict.get (key - 1) model.history

                historyX v =
                    Tuple.first <| Tuple.first v

                historyY v =
                    Tuple.second <| Tuple.first v

                historyColor v =
                    Tuple.second v
            in
            List.map
                (\n ->
                    String.fromInt n
                        ++ ",("
                        ++ String.fromInt (historyX (historyValue n))
                        ++ ","
                        ++ String.fromInt (historyY (historyValue n))
                        ++ "),"
                        ++ historyColor (historyValue n)
                )
            <|
                List.range 0 (Dict.size model.history)

        subPaletteData : List String
        subPaletteData =
            List.map
                (\n ->
                    String.fromInt n
                        ++ ","
                        ++ getSubPaletteColor model.subPalette n
                )
            <|
                List.range 0 (Dict.size model.subPalette - 1)

        aEn =
            ( ( JE.int, JE.int ), JE.string )
    in
    JE.encode 4 <|
        JE.object
            [ ( "general"
              , JE.object
                    [ ( "isSOSOGUSavedata", JE.bool True )
                    , ( "version", JE.string "1.0" )
                    ]
              )
            , ( "setting"
              , JE.object
                    [ ( "borderColor", JE.string model.setting.borderColor )
                    , ( "borderStyle", JE.string model.setting.borderStyle )
                    , ( "width", JE.string model.setting.width )
                    , ( "height", JE.string model.setting.height )
                    , ( "panelPosition"
                      , JE.object
                            [ ( "settingPanel"
                              , JE.string <|
                                    case model.setting.panelPosition.settingPanel of
                                        Right ->
                                            "Right"

                                        Left ->
                                            "Left"
                              )
                            , ( "palettePanel"
                              , JE.string <|
                                    case model.setting.panelPosition.palettePanel of
                                        Right ->
                                            "Right"

                                        Left ->
                                            "Left"
                              )
                            , ( "campusPanel"
                              , JE.string <|
                                    case model.setting.panelPosition.campusPanel of
                                        TopCenter ->
                                            "TopCenter"

                                        TopRight ->
                                            "TopRight"

                                        TopLeft ->
                                            "TopLeft"
                              )
                            ]
                      )
                    ]
              )
            , ( "campusSize"
              , JE.object
                    [ ( "width", JE.int model.campusSize.width )
                    , ( "height", JE.int model.campusSize.height )
                    ]
              )
            , ( "campus", JE.list JE.string campusData )
            , ( "didCreateCampus", JE.bool model.didCreateCampus )
            , ( "mainPalette", JE.string model.mainPalette )
            , ( "subPalette", JE.list JE.string subPaletteData )
            , ( "history", JE.list JE.string historyData )
            , ( "toolsSetting"
              , JE.object
                    [ ( "isDisplayDlButton", JE.bool model.toolsSetting.isDisplayDlButton ) ]
              )
            ]



--VIEW--


view : Model -> Html Msg
view model =
    div
        [ HAttrs.style "height" "100%"
        ]
        [ viewSaveWindow model
        , viewConfirmSaveCampus model
        , viewSaveAndNewCampusModalWindow model
        , layout
            []
          <|
            column [ E.width E.fill, E.height E.fill ]
                [ E.row
                    [ E.width E.fill
                    , E.height <| px 50
                    , Background.color <| rouIro
                    , paddingXY 16 0
                    , Border.widthEach { top = 1, right = 1, left = 1, bottom = 0 }
                    , Border.color <| shiroIro
                    ]
                    [ E.row
                        [ E.alignLeft
                        , Font.color <| shiroIro
                        , htmlAttribute <| HAttrs.style "letter-spacing" "0.06em"
                        , E.spacing 5
                        ]
                        [ E.image
                            [ E.width <| px 25
                            , E.height <| px 25
                            ]
                            { src = "file/icon.png"
                            , description = ""
                            }
                        , E.text "SOSOGU"
                        ]
                    , E.row
                        [ E.alignRight
                        , E.spacing 6
                        ]
                        [ newTabLink []
                            { url = "https:/floyd33n.github.io"
                            , label =
                                E.row [ E.spacing 3 ]
                                    [ E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)" ]
                                        { src = "file/home.svg"
                                        , description = ""
                                        }
                                    , E.el
                                        [ Font.color <| shiroIro
                                        , Font.size <| 15
                                        , htmlAttribute <| HAttrs.style "letter-spacing" "0.05em"
                                        ]
                                      <|
                                        E.text "Home"
                                    ]
                            }
                        , newTabLink []
                            { url = "https://github.com/floyd33n/SOSOGU"
                            , label =
                                E.row [ E.spacing 3 ]
                                    [ E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)" ]
                                        { src = "file/mark-github.svg"
                                        , description = ""
                                        }
                                    , E.el
                                        [ Font.color <| shiroIro
                                        , Font.size <| 15
                                        , htmlAttribute <| HAttrs.style "letter-spacing" "0.05em"
                                        ]
                                      <|
                                        E.text "Repository"
                                    ]
                            }
                        ]
                    ]
                , viewPanels model
                ]
        , css <| "../style.css"
        , createCampusWindow model
        ]


viewPanels : Model -> Element Msg
viewPanels model =
    let
        view_ : List (Element Msg)
        view_ =
            case
                ( model.setting.panelPosition.settingPanel
                , model.setting.panelPosition.palettePanel
                )
            of
                ( Left, Right ) ->
                    [ viewSettingPanel model
                    , viewCampusPanel model
                    , viewPalettePanel model
                    ]

                ( Right, Left ) ->
                    [ viewPalettePanel model
                    , viewCampusPanel model
                    , viewSettingPanel model
                    ]

                ( Right, Right ) ->
                    [ viewCampusPanel model
                    , viewPalettePanel model
                    , viewSettingPanel model
                    ]

                ( Left, Left ) ->
                    [ viewSettingPanel model
                    , viewPalettePanel model
                    , viewCampusPanel model
                    ]
    in
    E.row
        [ E.width E.fill
        , E.height E.fill
        ]
        view_


viewCampusPanel : Model -> Element Msg
viewCampusPanel model =
    column
        [ E.width E.fill
        , E.height E.fill
        , Background.color <| shironezuIro
        ]
        [ viewToolsPanel model
        , E.el (padding 3 :: campusPosition model.setting) <|
            html <|
                viewCampus model ( model.campusSize.width, model.campusSize.height )
        ]


campusPosition : Setting -> List (E.Attribute Msg)
campusPosition setting =
    case setting.panelPosition.campusPanel of
        TopCenter ->
            [ centerX
            , alignTop
            ]

        TopRight ->
            [ E.alignRight
            , alignTop
            ]

        TopLeft ->
            [ E.alignLeft
            , alignTop
            ]


viewToolsPanel : Model -> Element Msg
viewToolsPanel model =
    let
        viewUndoButton : Element Msg
        viewUndoButton =
            Input.button
                [ Border.color shiroIro
                , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
                , E.height <| px 18
                ]
                { onPress =
                    let
                        x =
                            Tuple.first (Tuple.first (Maybe.withDefault ( ( 0, 0 ), "white" ) (Dict.get (Dict.size model.history - 1) model.history)))

                        y =
                            Tuple.second (Tuple.first (Maybe.withDefault ( ( 0, 0 ), "white" ) (Dict.get (Dict.size model.history - 1) model.history)))
                    in
                    Just <| Undo ( y, x )
                , label =
                    E.el
                        [ Font.color <| shiroIro
                        , Font.size <| 14
                        , centerY
                        ]
                    <|
                        E.text <|
                            "Undo"
                }

        saveButton : Element Msg
        saveButton =
            Input.button
                [ Border.color <| shiroIro
                , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
                , E.height <| px 18
                ]
                { onPress = Just ShowSaveWindow
                , label =
                    E.el
                        [ Font.color <| shiroIro
                        , Font.size <| 14
                        , centerY
                        ]
                    <|
                        E.text <|
                            "Save"
                }

        newButton : Element Msg
        newButton =
            Input.button
                [ Border.color <| shiroIro
                , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
                , E.height <| px 18
                ]
                { onPress = Just NewProject
                , label =
                    E.el
                        [ Font.color <| shiroIro
                        , Font.size <| 14
                        , centerY
                        ]
                    <|
                        E.text <|
                            "New"
                }
    in
    E.row
        [ E.width E.fill
        , E.height <| px 36
        , case ( model.setting.panelPosition.settingPanel, model.setting.panelPosition.palettePanel ) of
            ( Right, Right ) ->
                Border.widthEach { top = 1, right = 0, left = 1, bottom = 1 }

            ( Left, Left ) ->
                Border.widthEach { top = 1, right = 1, left = 0, bottom = 1 }

            ( Right, Left ) ->
                Border.widthEach { top = 1, right = 0, left = 0, bottom = 1 }

            ( Left, Right ) ->
                Border.widthEach { top = 1, right = 0, left = 0, bottom = 1 }
        , Border.color <| shiroIro
        , Background.color <| rouIro
        ]
        [ E.el
            [ Font.color <| shiroIro
            , Font.size <| 16
            , padding 2
            , centerY
            , htmlAttribute <| HAttrs.style "letter-spacing" "0.05em"
            , htmlAttribute <| HAttrs.class "button hint-bottom"
            , htmlAttribute <| HAttrs.property "aria-label" (JE.string "Bottom")
            ]
          <|
            E.row
                [ E.spacing 2
                ]
                [ E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)" ]
                    { src = "file/tools.svg"
                    , description = ""
                    }
                , E.text "Tools"
                ]
        , E.row
            [ E.alignRight
            , paddingXY 20 0
            , E.spacing 10
            ]
            [ newButton
            , verticalLine
            , saveButton
            , verticalLine
            , viewUndoButton
            ]
        ]


verticalLine : Element Msg
verticalLine =
    E.el
        [ E.width <| px 1
        , E.height <| px 16
        , Border.color <| shiroIro
        , Border.widthEach { top = 0, right = 1, left = 0, bottom = 0 }
        ]
    <|
        E.text ""


panelHr : Element Msg
panelHr =
    E.el
        [ centerX
        , E.width <| px 80
        , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
        , Border.color <| shiroIro
        ]
    <|
        E.none


viewSettingPanel : Model -> Element Msg
viewSettingPanel model =
    case model.settingPanelStatus of
        Open ->
            openSettingPanel model

        Close ->
            closedSettingPanel model


closedSettingPanel : Model -> Element Msg
closedSettingPanel model =
    column
        [ E.width <| px 50
        , E.height E.fill
        , Border.width 1
        , Border.color <| shiroIro
        , Background.color <| rouIro
        , E.spacing 2
        , padding 1
        ]
        [ el
            [ centerX
            , padding 1
            ]
          <|
            Input.button
                [ Border.color <| shiroIro
                , Border.width <| 1
                , Border.rounded 1
                , htmlAttribute <| HAttrs.style "padding" "2px"
                ]
                { onPress = Just OpenSettingPanel
                , label =
                    el [ centerX ] <|
                        E.image
                            [ htmlAttribute <| HAttrs.style "filter" "invert(100%)"
                            , E.width <| px 15
                            , E.height <| px 15
                            ]
                            { src = "file/settings.svg"
                            , description = ""
                            }
                }
        ]


openSettingPanel : Model -> Element Msg
openSettingPanel model =
    column
        [ E.width <| px 110
        , E.height E.fill
        , Border.width 1
        , Border.color <| shiroIro
        , Background.color <| rouIro
        ]
        [ el
            [ Font.color <| shiroIro
            , Font.size <| 16
            , centerX
            , padding 2
            , htmlAttribute <| HAttrs.style "letter-spacing" "0.05em"
            ]
          <|
            E.el [] <|
                Input.button
                    []
                    { onPress = Just CloseSettingPanel
                    , label =
                        E.el [ centerX ] <|
                            E.row [ E.spacing 2 ]
                                [ E.el
                                    [ Border.color <| shiroIro
                                    , Border.width <| 1
                                    , Border.rounded 1
                                    , htmlAttribute <| HAttrs.style "padding" "0px"
                                    ]
                                  <|
                                    E.image
                                        [ htmlAttribute <| HAttrs.style "filter" "invert(100%)"
                                        , E.width <| px 17
                                        , E.height <| px 17
                                        ]
                                        { src = "file/x.svg"
                                        , description = ""
                                        }
                                , E.text "Setting"
                                ]
                    }
        , panelHr

        -- Border Color --
        , column
            [ centerX
            , E.spacing 4
            , padding 3
            ]
            [ E.el
                [ Font.size 14
                , Font.color <| shiroIro
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Border Color"
            , panelHr
            , E.el
                [ centerX
                ]
              <|
                html <|
                    BInput.text
                        [ BInput.onInput BorderColorValue
                        , if isColor model.tempSetting.borderColor then
                            BInput.attrs []

                          else
                            BInput.danger
                        , BInput.attrs
                            [ HAttrs.style "height" "14px"
                            , HAttrs.style "width" "80px"
                            , HAttrs.style "font-size" "0.7em"
                            ]
                        ]
            , el [ centerX ] <|
                html <|
                    div
                        [ HAttrs.style "color" "#e2041b"
                        , HAttrs.style "font-size" "13px"
                        ]
                        [ H.text <|
                            if isColor model.tempSetting.borderColor then
                                ""

                            else if String.isEmpty model.tempSetting.borderColor then
                                "Is Empty"

                            else
                                "Isn't Color"
                        ]
            ]

        -- Border Style --
        , column
            [ centerX
            , padding 3
            , E.spacing 5
            ]
            [ E.el
                [ Font.size 14
                , Font.color <| shiroIro
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Border Style"
            , panelHr
            , E.el [] <|
                html <|
                    let
                        item_ : String -> String -> BSelect.Item msg
                        item_ value_ text_ =
                            BSelect.item
                                [ HAttrs.value value_
                                , HAttrs.style "height" "13px"
                                , HAttrs.style "position" "relative"
                                , HAttrs.style "text-align" "center"
                                , HAttrs.style "font-size" "13px"
                                , HAttrs.style "padding" "0px"
                                ]
                                [ H.text text_ ]
                    in
                    BSelect.select
                        [ BSelect.onChange Change
                        , BSelect.small
                        , BSelect.attrs
                            [ HAttrs.style "height" "18px"
                            , HAttrs.style "width" "80px"
                            , HAttrs.style "font-size" "13px"
                            , HAttrs.style "padding" "0px"
                            ]
                        ]
                        [ item_ "solid" "solid"
                        , item_ "none" "none"
                        , item_ "dotted" "dotted"
                        , item_ "dashed" "dashed"
                        ]
            ]

        -- Pixel Size --
        , column
            [ centerX
            , padding 3
            , E.spacing 5
            ]
            [ E.el
                [ Font.size 14
                , Font.color <| shiroIro
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Pixel Size"
            , panelHr
            , column [ E.spacing 3 ] <|
                let
                    pixelSizeErr : String -> String
                    pixelSizeErr value_ =
                        case String.toInt value_ of
                            Nothing ->
                                if String.isEmpty value_ then
                                    "Is empty"

                                else
                                    "Isn't Integer"

                            Just n ->
                                if n <= 0 then
                                    "Is 0 or less"

                                else
                                    ""
                in
                [ E.row
                    [ Font.color <| shiroIro
                    , Font.size <| 14
                    , centerX
                    ]
                    [ E.el [] <|
                        E.text "width : "
                    , html <|
                        BInput.text
                            [ BInput.onInput SetPixelWidth
                            , BInput.small
                            , if Maybe.withDefault 0 (String.toInt model.tempSetting.width) > 0 then
                                BInput.attrs []

                              else
                                BInput.danger
                            , BInput.attrs
                                [ HAttrs.style "height" "14px"
                                , HAttrs.style "width" "40px"
                                , HAttrs.style "font-size" "95%"
                                ]
                            ]
                    ]
                , html <|
                    div
                        [ HAttrs.style "color" "#e2041b"
                        , HAttrs.style "font-size" "13px"
                        ]
                        [ H.text <| pixelSizeErr model.tempSetting.width ]
                , E.row
                    [ Font.color <| shiroIro
                    , Font.size <| 14
                    , centerX
                    ]
                    [ E.el [] <|
                        E.text "height : "
                    , html <|
                        BInput.text
                            [ BInput.onInput SetPixelHeight
                            , BInput.small
                            , if Maybe.withDefault 0 (String.toInt model.tempSetting.height) > 0 then
                                BInput.attrs []

                              else
                                BInput.danger
                            , BInput.attrs
                                [ HAttrs.style "height" "14px"
                                , HAttrs.style "width" "40px"
                                , HAttrs.style "font-size" "95%"
                                ]
                            ]
                    ]
                , html <|
                    div
                        [ HAttrs.style "color" "#e2041b"
                        , HAttrs.style "font-size" "13px"
                        ]
                        [ H.text <| pixelSizeErr model.tempSetting.height ]
                ]
            ]

        --panel position--
        , column
            [ centerX
            , E.spacing 10
            ]
            [ E.el
                [ Font.size 14
                , Font.color <| shiroIro
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Position"
            , panelHr
            , column
                [ Font.size 14
                , Font.color <| shiroIro
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03.em"
                , E.spacing 7
                ]
                [ column
                    [ centerX
                    , E.spacing 4
                    ]
                    [ E.el [ centerX ] <|
                        E.text "Setting"
                    , E.el [ centerX ] <|
                        html <|
                            viewSettingPalettePositionSelect SettingPanel model.tempSetting
                    ]
                , column
                    [ centerX
                    , E.spacing 4
                    ]
                    [ E.el [ centerX ] <|
                        E.text "Palette"
                    , E.el [ centerX ] <|
                        html <|
                            viewSettingPalettePositionSelect PalettePanel model.tempSetting
                    ]
                , column
                    [ centerX
                    , E.spacing 4
                    ]
                    [ E.el [ centerX ] <|
                        E.text "Campus"
                    , E.el [ centerX ] <|
                        html <|
                            viewCampusPositionSetting model.tempSetting
                    ]
                ]
            ]
        , E.el
            [ centerX
            , paddingEach { top = 20, right = 0, left = 0, bottom = 0 }
            ]
          <|
            if isCorrectSetting model.tempSetting then
                Input.button
                    [ htmlAttribute <| HAttrs.style "color" "white"
                    , Border.color <| shiroIro
                    , Border.width <| 2
                    , Border.rounded 5
                    , E.width <| px 60
                    , E.height <| px 30
                    ]
                    { onPress = Just ApplySetting
                    , label =
                        E.el
                            [ Font.color <| shiroIro
                            , Font.size <| 14
                            , centerY
                            , centerX
                            ]
                        <|
                            E.text "Apply"
                    }

            else
                Input.button
                    [ Border.color <| shiroIro
                    , Border.width <| 2
                    , Border.rounded 5
                    , E.alpha 0.6
                    , E.width <| px 60
                    , E.height <| px 30
                    ]
                    { onPress = Nothing
                    , label =
                        E.el
                            [ Font.color <| shiroIro
                            , Font.size <| 14
                            , Font.strike
                            , centerY
                            , centerX
                            ]
                        <|
                            E.text "Apply"
                    }
        ]


viewCampusPositionSetting : Setting -> Html Msg
viewCampusPositionSetting tempSetting =
    let
        tempDiv : CampusPosition -> Html Msg
        tempDiv position_ =
            div
                [ HEvents.onClick (SetCampusPosition position_)
                , HAttrs.style "border" "none"
                , HAttrs.style "width" "15px"
                , HAttrs.style "height" "15px"
                , if tempSetting.panelPosition.campusPanel == position_ then
                    HAttrs.style "background-color" "#f0ad00"

                  else
                    HAttrs.style "background-color" "white"
                , HAttrs.style "float" "left"
                , HAttrs.style "margin" "-1px"
                , case position_ of
                    TopLeft ->
                        HAttrs.style "border-radius" "5px 0 0 5px"

                    TopRight ->
                        HAttrs.style "border-radius" "0 5px 5px 0"

                    TopCenter ->
                        HAttrs.style "" ""
                ]
                []
    in
    div [ HAttrs.style "float" "left" ]
        [ tempDiv TopLeft
        , tempDiv TopCenter
        , tempDiv TopRight
        ]


viewSettingPalettePositionSelect : Panel -> Setting -> Html Msg
viewSettingPalettePositionSelect panel_ tempSetting =
    let
        temp_ : Position -> Html Msg
        temp_ position_ =
            div
                [ HEvents.onClick (ChangePanelPosition panel_ position_)
                , HAttrs.style "border" "none"
                , HAttrs.style "width" "15px"
                , HAttrs.style "height" "15px"
                , case panel_ of
                    SettingPanel ->
                        if tempSetting.panelPosition.settingPanel == position_ then
                            HAttrs.style "background-color" "#60b5cc"

                        else
                            HAttrs.style "background-color" "white"

                    PalettePanel ->
                        if tempSetting.panelPosition.palettePanel == position_ then
                            HAttrs.style "background-color" "#7fd13b"

                        else
                            HAttrs.style "background-color" "white"
                , HAttrs.style "float" "left"
                , HAttrs.style "margin" "-1px"
                , if position_ == Left then
                    HAttrs.style "border-radius" "5px 0 0 5px"

                  else if position_ == Right then
                    HAttrs.style "border-radius" "0 5px 5px 0"

                  else
                    HAttrs.style "" ""
                ]
                []
    in
    div [ HAttrs.style "float" "left" ]
        [ div []
            [ temp_ Left
            , temp_ Right
            ]
        ]


viewPalettePanel : Model -> Element Msg
viewPalettePanel model =
    column
        [ E.width <| px 110
        , E.height E.fill
        , case model.setting.panelPosition.palettePanel of
            Right ->
                case model.setting.panelPosition.settingPanel of
                    Right ->
                        Border.widthEach { top = 1, right = 0, left = 1, bottom = 1 }

                    Left ->
                        Border.width 1

            Left ->
                case model.setting.panelPosition.settingPanel of
                    Right ->
                        Border.width 1

                    Left ->
                        Border.widthEach { top = 1, right = 1, left = 0, bottom = 1 }
        , Border.color <| shiroIro
        , Background.color <| rouIro
        ]
        [ E.row
            [ Font.color <| shiroIro
            , Font.size <| 16
            , centerX
            , E.padding 2
            , htmlAttribute <| HAttrs.style "letter-spacing" "0.05em"
            , E.spacing 2
            ]
            [ E.image
                [ htmlAttribute <| HAttrs.style "filter" "invert(100%)"
                ]
                { src = "file/palette.svg"
                , description = ""
                }
            , E.text "Palette"
            ]
        , panelHr
        , column
            [ centerX
            , padding 3
            , E.spacing 5
            ]
            [ E.el
                [ Font.color <| shiroIro
                , Font.size <| 14
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Add Color"
            , panelHr
            , E.el
                [ centerX
                ]
              <|
                html <|
                    BInput.text
                        [ BInput.onInput ColorValue
                        , if isColor model.colorValue then
                            BInput.attrs []

                          else if String.isEmpty model.colorValue then
                            BInput.attrs []

                          else
                            BInput.danger
                        , BInput.attrs
                            [ HAttrs.style "height" "14px"
                            , HAttrs.style "width" "80px"
                            , HAttrs.style "font-size" "0.7em"
                            ]
                        ]
            , el [ centerX ] <|
                html <|
                    div
                        [ HAttrs.style "color" "#e2041b"
                        , HAttrs.style "font-size" "13px"
                        ]
                        [ H.text <|
                            if isColor model.colorValue then
                                ""

                            else if String.isEmpty model.colorValue then
                                ""

                            else
                                "Isn't Color"
                        ]
            , el [ centerX ] <|
                if isColor <| model.colorValue then
                    Input.button
                        [ htmlAttribute <| HAttrs.style "color" "white"
                        , Border.color <| shiroIro
                        , Border.width <| 2
                        , Border.rounded 5
                        , E.width <| px 60
                        , E.height <| px 30
                        ]
                        { onPress = Just (AddColorToSubPalette model.colorValue)
                        , label =
                            E.row
                                [ centerX
                                , centerY
                                ]
                                [ E.el
                                    [ Font.color <| rouIro
                                    , Font.size <| 14
                                    , Font.color <| shiroIro
                                    , centerX
                                    , centerY
                                    ]
                                  <|
                                    E.text "Add "
                                , html <|
                                    div
                                        [ HAttrs.style "width" "14px"
                                        , HAttrs.style "height" "14px"
                                        , HAttrs.style "background-color" model.colorValue
                                        ]
                                        []
                                ]
                        }

                else
                    E.el [ E.alpha 0.6 ] <|
                        Input.button
                            [ Region.description "Add"
                            , htmlAttribute <| HAttrs.style "color" "white"
                            , Border.color <| shiroIro
                            , Border.width <| 2
                            , Border.rounded 5
                            , E.width <| px 60
                            , E.height <| px 30
                            ]
                            { onPress = Nothing
                            , label =
                                E.row
                                    [ centerX
                                    , centerY
                                    ]
                                    [ E.el
                                        [ Font.color <| rouIro
                                        , Font.size <| 14
                                        , Font.color <| shiroIro
                                        , Font.strike
                                        ]
                                      <|
                                        E.text "Add "
                                    , html <|
                                        div
                                            [ HAttrs.style "width" "14px"
                                            , HAttrs.style "height" "14px"
                                            , HAttrs.style "background-color" "white"
                                            ]
                                            []
                                    ]
                            }
            ]
        , column
            [ centerX
            , E.padding 3
            , E.spacing 5
            ]
            [ E.el
                [ Font.color <| shiroIro
                , Font.size <| 14
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Main Palette"
            , panelHr
            , E.el
                [ centerX
                ]
              <|
                html <|
                    div
                        [ HAttrs.style "width" "30px"
                        , HAttrs.style "height" "30px"
                        , HAttrs.style "border" "solid 1px black"
                        , HAttrs.style "background-color" model.mainPalette
                        ]
                        []
            ]
        , column
            [ E.paddingEach { top = 2, right = 0, left = 0, bottom = 2 }
            , E.spacing 5
            , centerX
            ]
            [ E.el
                [ Font.color <| shiroIro
                , Font.size <| 14
                , centerX
                , htmlAttribute <| HAttrs.style "letter-spacing" "0.03em"
                ]
              <|
                E.text "Sub Palette"
            , panelHr
            , column
                [ centerX
                ]
                [ wrappedRow
                    [ E.spacing 3
                    , E.width <| px 81
                    ]
                  <|
                    List.map
                        (\plt ->
                            E.el [] <|
                                html <|
                                    div
                                        [ HAttrs.style "width" "25px"
                                        , HAttrs.style "height" "25px"
                                        , HAttrs.style "background-color" <| getSubPaletteColor model.subPalette (plt - 1)
                                        , HAttrs.style "border" "solid 1px black"
                                        , onDoubleClick (DeleteSubPalette plt)
                                        , HEvents.onClick (SetMainPalette (plt - 1))
                                        ]
                                        []
                        )
                    <|
                        List.range 1 (Dict.size model.subPalette)
                ]
            ]
        ]


isCorrectSetting : Setting -> Bool
isCorrectSetting setting =
    isColor setting.borderColor && isCorrectWidthHeight setting.width setting.height


viewConfirmSaveCampus : Model -> Html Msg
viewConfirmSaveCampus model =
    BGrid.container []
        [ BModal.config CloseSaveEditingCampusModalWindow
            |> BModal.hideOnBackdropClick True
            |> BModal.h5 [] [ H.text "Save Editing Campus?" ]
            |> BModal.body []
                [ BGrid.containerFluid []
                    [ BGrid.row []
                        [ BGrid.col
                            [ BCol.xs6 ]
                            [ div []
                                [ BBtn.button
                                    [ BBtn.outlineSecondary
                                    , BBtn.secondary
                                    , BBtn.onClick (SaveEditingCampusModalWindow No)
                                    ]
                                    [ H.text "No" ]
                                ]
                            ]
                        , BGrid.col
                            [ BCol.xs6 ]
                            [ div []
                                [ BBtn.button
                                    [ BBtn.outlinePrimary
                                    , BBtn.primary
                                    , BBtn.onClick (SaveEditingCampusModalWindow Yes)
                                    ]
                                    [ H.text "Yes" ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> BModal.view model.saveEditingCampusModalWindow
        ]


viewSaveAndNewCampusModalWindow : Model -> Html Msg
viewSaveAndNewCampusModalWindow model =
    BGrid.container []
        [ BModal.config CloseSaveWindow
            |> BModal.hideOnBackdropClick True
            |> BModal.h5 [] [ H.text "Save" ]
            |> BModal.body []
                [ BGrid.containerFluid []
                    [ BGrid.row []
                        [ BGrid.col
                            [ BCol.xs6 ]
                            [ div [] [ H.text "Save Project" ]
                            , div []
                                [ BBtn.button
                                    [ BBtn.outlinePrimary
                                    , BBtn.primary
                                    , BBtn.onClick DLSavedata
                                    ]
                                    [ H.text "DL Savedata" ]
                                ]
                            ]
                        , BGrid.col
                            [ BCol.xs6 ]
                            [ div [] [ H.text "Save Image" ]
                            , div [ HAttrs.id "downloadImage" ]
                                [ BBtn.button
                                    [ BBtn.outlinePrimary
                                    , BBtn.primary
                                    , BBtn.onClick DLImage
                                    ]
                                    [ H.text "DL Image" ]
                                ]
                            ]
                        ]
                    , BGrid.row []
                        [ BGrid.col
                            [ BCol.xs6 ]
                            [ div [] [ H.text "New Campus" ]
                            , div []
                                [ BBtn.button
                                    [ BBtn.outlinePrimary
                                    , BBtn.primary
                                    , BBtn.onClick (SaveEditingCampusModalWindow No)
                                    ]
                                    [ H.text "New Campus" ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> BModal.view model.saveAndNewCampusModalWindow
        ]


viewSaveWindow : Model -> Html Msg
viewSaveWindow model =
    BGrid.container []
        [ BModal.config CloseSaveWindow
            |> BModal.hideOnBackdropClick True
            |> BModal.h5 [] [ H.text "Save" ]
            |> BModal.body []
                [ BGrid.containerFluid []
                    [ BGrid.row []
                        [ BGrid.col
                            [ BCol.xs6 ]
                            [ div [] [ H.text "Save Project" ]
                            , div []
                                [ BBtn.button
                                    [ BBtn.outlinePrimary
                                    , BBtn.primary
                                    , BBtn.onClick DLSavedata
                                    ]
                                    [ H.text "DL Savedata" ]
                                ]
                            ]
                        , BGrid.col
                            [ BCol.xs6 ]
                            [ div [] [ H.text "Save Image" ]
                            , div [ HAttrs.id "downloadImage" ]
                                [ BBtn.button
                                    [ BBtn.outlinePrimary
                                    , BBtn.primary
                                    , BBtn.onClick DLImage
                                    , BBtn.disabled <| not model.didCreateCampus
                                    ]
                                    [ H.text "DL Image" ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> BModal.view model.saveModalWindow
        ]


createCampusWindow : Model -> Html Msg
createCampusWindow model =
    let
        viewLoadSavedataErr : Html Msg
        viewLoadSavedataErr =
            case model.loadedSavedata of
                "" ->
                    H.text ""

                _ ->
                    if isSOSOGUSavedata model.loadedSavedata then
                        H.text ""

                    else
                        BAlert.simpleDanger
                            [ HAttrs.style "margin" "10px"
                            ]
                            [ div
                                []
                                [ H.text "Bad Savedata" ]
                            ]
    in
    BGrid.container []
        [ BModal.config CloseCreateCampusWindow
            |> BModal.hideOnBackdropClick False
            |> BModal.small
            |> BModal.h5
                [ HAttrs.style "padding-left" "20px" ]
                [ H.text "Let's Start SOSOGU" ]
            |> BModal.body []
                [ BGrid.containerFluid []
                    [ BGrid.row []
                        [ BGrid.col [ BCol.xs6 ]
                            [ div [ HAttrs.style "margin" "0 auto" ]
                                [ H.text "Width" ]
                            , BInput.number
                                [ BInput.small
                                , BInput.onInput SetCampusWidth
                                ]
                            ]
                        , BGrid.col [ BCol.xs5 ]
                            [ div [ HAttrs.style "margin" "0 auto" ]
                                [ H.text "Height" ]
                            , BInput.number
                                [ BInput.small
                                , BInput.onInput SetCampusHeight
                                ]
                            ]
                        ]
                    , BGrid.row []
                        [ BGrid.col
                            [ BCol.textAlign BText.alignXsCenter
                            , BCol.attrs [ BUtilsSpacing.p2 ]
                            ]
                            [ BBtn.button
                                [ BBtn.outlinePrimary
                                , BBtn.primary
                                , BBtn.onClick CreateCampus
                                , BBtn.disabled <| not (isCorrectWidthHeight model.tempCampusSize.width model.tempCampusSize.height)
                                ]
                                [ H.text "Create Campus" ]
                            ]
                        ]
                    , BGrid.row []
                        [ BGrid.col
                            [ BCol.textAlign BText.alignXsCenter
                            , BCol.attrs []
                            ]
                            [ div [] [ H.text "or" ] ]
                        ]
                    , BGrid.row []
                        [ BGrid.col
                            [ BCol.textAlign BText.alignXsCenter
                            , BCol.attrs [ BUtilsSpacing.p2 ]
                            ]
                            [ BBtn.button
                                [ BBtn.outlinePrimary
                                , BBtn.secondary
                                , BBtn.onClick UpSavedata
                                ]
                                [ H.text "Load Savedata" ]
                            ]
                        ]
                    , BGrid.row []
                        [ BGrid.col
                            [ BCol.textAlign BText.alignXsCenter
                            ]
                            [ div [] [ viewLoadSavedataErr ]
                            ]
                        ]
                    ]
                ]
            |> BModal.view model.openingModalWindow
        ]



--DEBUG--
--FUNC--


getSubPaletteColor : SubPalette -> Serial -> CssColor
getSubPaletteColor subPalette n =
    Maybe.withDefault "white" (Dict.get n subPalette)


getCampusColor : Campus -> Point -> String
getCampusColor campus ( x, y ) =
    Maybe.withDefault "white" (Dict.get ( x, y ) campus)


viewCampusS : Model -> Points -> Html Msg
viewCampusS model ( width, height ) =
    if model.didCreateCampus then
        div [ HAttrs.id "campus" ]
            [ div [] <|
                List.map
                    (\y ->
                        div [] <|
                            List.map
                                (\x ->
                                    div
                                        [ HAttrs.style "float" "left"
                                        ]
                                    <|
                                        [ div
                                            [ HAttrs.style "padding-right" (model.setting.width ++ "px")
                                            , HAttrs.style "padding-top" (model.setting.height ++ "px")
                                            , HAttrs.style "border" (model.setting.borderColor ++ " " ++ model.setting.borderStyle)
                                            , HAttrs.style "background-color" (getCampusColor model.campus ( x, y ))
                                            , HAttrs.style "margin" "-1px"
                                            , HEvents.onClick (ChangeColor ( x, y ) model.mainPalette)
                                            ]
                                            []
                                        ]
                                )
                            <|
                                List.range 0 (width - 1)
                    )
                <|
                    List.range 0 (height - 1)
            ]

    else
        div [] []


viewCampus : Model -> Points -> Html Msg
viewCampus model ( width, height ) =
    let
        strokeAttrs : BorderStyle -> CssColor -> List (Svg.Attribute msg)
        strokeAttrs style color =
            case style of
                "solid" ->
                    [ SAttrs.stroke color
                    ]

                "none" ->
                    [ SAttrs.stroke "" ]

                "dashed" ->
                    [ SAttrs.stroke color
                    , SAttrs.strokeDasharray "4"
                    ]

                "dotted" ->
                    [ SAttrs.stroke color
                    , SAttrs.strokeDasharray "1"
                    ]

                _ ->
                    [ SAttrs.stroke "red" ]
    in
    if model.didCreateCampus then
        div
            [ HAttrs.id "campus"
            ]
            [ div [] <|
                List.map
                    (\y ->
                        div [] <|
                            List.map
                                (\x ->
                                    div
                                        [ HAttrs.style "float" "left"
                                        , HAttrs.style "width" ("calc(1px + " ++ model.setting.width ++ "px)")
                                        , HAttrs.style "height" ("calc(1px + " ++ model.setting.height ++ "px)")
                                        , HAttrs.style "margin" "-1px"
                                        , HAttrs.style "display" "flex"
                                        ]
                                    <|
                                        [ Svg.svg
                                            [ SAttrs.viewBox ("0, 0, " ++ model.setting.width ++ ", " ++ model.setting.height)
                                            , SAttrs.width (model.setting.width ++ "px")
                                            , SAttrs.height (model.setting.height ++ "px")
                                            ]
                                            [ rect
                                                (List.append
                                                    [ SAttrs.strokeWidth "1"
                                                    , SAttrs.fill <| getCampusColor model.campus ( x, y )
                                                    , SAttrs.width (model.setting.width ++ "px")
                                                    , SAttrs.height (model.setting.height ++ "px")
                                                    , SEvents.onClick (ChangeColor ( x, y ) model.mainPalette)
                                                    , SAttrs.x "0"
                                                    , SAttrs.y "0"
                                                    ]
                                                    (strokeAttrs model.setting.borderStyle model.setting.borderColor)
                                                )
                                                []
                                            ]
                                        ]
                                )
                            <|
                                List.range 0 (width - 1)
                    )
                <|
                    List.range 0 (height - 1)
            ]

    else
        div [] []


addColorToSubPalette : SubPalette -> CssColor -> SubPalette
addColorToSubPalette subPalette color =
    let
        tempSubPalette =
            subPalette
                |> DictEx.mapKeys (\n -> n + 1)
    in
    Dict.insert
        0
        color
        tempSubPalette


displayPalette : SubPalette -> Html Msg
displayPalette subPalette =
    div [] <|
        List.map
            (\plt ->
                div []
                    [ div
                        [ HAttrs.id "palette_square"
                        , HEvents.onClick <| SetMainPalette (plt - 1)
                        , HAttrs.style "background-color" <| getSubPaletteColor subPalette (plt - 1)
                        ]
                        []
                    , div [ HAttrs.id "palette_color_name" ]
                        []
                    ]
            )
        <|
            List.range 1 (Dict.size subPalette)


isColor : String -> Bool
isColor exValue =
    case String.left 1 exValue of
        "#" ->
            xor
                (Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "[g-z]")
                    (String.dropLeft 1 exValue)
                )
            <|
                String.length exValue
                    == 4
                    || String.length exValue
                    == 7

        _ ->
            let
                cssColorNames =
                    [ "aliceblue"
                    , "antiquewhite"
                    , "aqua"
                    , "aquamarine"
                    , "azure"
                    , "beige"
                    , "bisque"
                    , "black"
                    , "blanchedalmond"
                    , "blue"
                    , "blueviolet"
                    , "brown"
                    , "burlywood"
                    , "cadetblue"
                    , "chartreuse"
                    , "chocolate"
                    , "coral"
                    , "cornflowerblue"
                    , "cornsilk"
                    , "crimson"
                    , "cyan"
                    , "darkblue"
                    , "darkcyan"
                    , "darkgoldenrod"
                    , "darkgray"
                    , "darkgrey"
                    , "darkgreen"
                    , "darkkhaki"
                    , "darkmagenta"
                    , "darkolivegreen"
                    , "darkorange"
                    , "darkorchid"
                    , "darkred"
                    , "darksalmon"
                    , "darkseagreen"
                    , "darkslateblue"
                    , "darkslategrey"
                    , "darkslategray"
                    , "darkturquoise"
                    , "darkviolet"
                    , "deeppink"
                    , "deepskyblue"
                    , "dimgray"
                    , "dimgrey"
                    , "dodgerblue"
                    , "firebrick"
                    , "floralwhite"
                    , "forestgreen"
                    , "fuchsia"
                    , "gainsboro"
                    , "ghostwhite"
                    , "gold"
                    , "goldenrod"
                    , "gray"
                    , "grey"
                    , "green"
                    , "greenyellow"
                    , "honeydew"
                    , "hotpink"
                    , "indianred"
                    , "indigo"
                    , "ivory"
                    , "khaki"
                    , "lavender"
                    , "lavenderblush"
                    , "lawngreen"
                    , "lemonchiffon"
                    , "lightblue"
                    , "lightcoral"
                    , "lightcyan"
                    , "lightgoldenrodyellow"
                    , "lightgray"
                    , "lightgrey"
                    , "lightgreen"
                    , "lightpink"
                    , "lightsalmon"
                    , "lightseagreen"
                    , "lightskyblue"
                    , "lightslategray"
                    , "lightslategrey"
                    , "lightsteelblue"
                    , "lightyellow"
                    , "lime"
                    , "limegreen"
                    , "linen"
                    , "magenta"
                    , "mediumaquamarine"
                    , "mediumblue"
                    , "mediumorchid"
                    , "mediumpurple"
                    , "mediumseagreen"
                    , "mediumslateblue"
                    , "mediumspringgreen"
                    , "mediumturquoise"
                    , "mediumvioletred"
                    , "midnightblue"
                    , "mintcream"
                    , "mistyrose"
                    , "moccasin"
                    , "navajowhite"
                    , "navy"
                    , "oldlace"
                    , "olive"
                    , "olivedrab"
                    , "orange"
                    , "orangered"
                    , "orchid"
                    , "palegoldenrod"
                    , "palegreen"
                    , "paleturquoise"
                    , "palevioletred"
                    , "papayawhip"
                    , "peachpuff"
                    , "peru"
                    , "pink"
                    , "plum"
                    , "powderblue"
                    , "purple"
                    , "rebeccapurple"
                    , "red"
                    , "rosybrown"
                    , "royalblue"
                    , "saddlebrown"
                    , "salmon"
                    , "sandybrown"
                    , "seagreen"
                    , "seashell"
                    , "sienna"
                    , "silver"
                    , "skyblue"
                    , "slateblue"
                    , "slategray"
                    , "slategrey"
                    , "snow"
                    , "springgreen"
                    , "steelblue"
                    , "tan"
                    , "teal"
                    , "thistle"
                    , "tomato"
                    , "turquoise"
                    , "violet"
                    , "wheat"
                    , "white"
                    , "whitesmoke"
                    , "yellow"
                    , "yellowgreen"
                    ]

                -- By https://www.w3schools.com/colors/colors_names.asp
                isColorName : Bool
                isColorName =
                    List.member exValue cssColorNames
            in
            isColorName


isCorrectWidthHeight : String -> String -> Bool
isCorrectWidthHeight width_ height_ =
    let
        chkInt : Bool
        chkInt =
            Maybe.withDefault 0 (String.toInt width_)
                * Maybe.withDefault 0 (String.toInt height_)
                > 0

        chkLength : Bool
        chkLength =
            Maybe.withDefault 0 (String.toInt width_)
                <= 64
                && Maybe.withDefault 0 (String.toInt height_)
                <= 64
    in
    chkInt && chkLength



--MAIN--


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    batch [ campusImageUrlToElm GetImageUrl, Time.every 1000 Tick ]


port generateCampusImage : () -> Cmd msg


port campusImageUrlToElm : (String -> msg) -> Sub msg



--ColorSet--


rouIro =
    rgb255 43 43 43


sumiIro =
    rgb255 89 88 87


shiroIro =
    rgb255 255 255 255


shironeriIro =
    rgb255 243 243 242


shironezuIro =
    rgb255 220 221 221
