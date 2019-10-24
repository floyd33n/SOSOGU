port module Main exposing (..)

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
import Debug exposing (..)
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
import Savedata exposing (..)
import Svg exposing (..)
import Svg.Attributes as SAttrs exposing (..)
import Svg.Events as SEvents exposing (..)
import Task exposing (..)
import Time exposing (..)
import Types exposing (..)
import Utilities exposing (..)
import View exposing (..)



--INIT--


init : () -> ( Model, Cmd Msg )
init _ =
    ( { campus = Dict.empty
      , colorValue = "white"
      , subPalette = Dict.fromList [ ( 0, "white" ) ]
      , mainPalette = "white"
      , campusSize = CampusSize 0 0
      , tempCampusSize = TempCampusSize "" ""
      , didCreateCampus = False
      , modalVisibility = BModal.hidden
      , openingModalWindow = BModal.shown
      , setting = initSetting
      , tempSetting = initSetting
      , borderColorValue = "black"
      , history = Dict.empty
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


initTimeGetter : TimeGetter
initTimeGetter =
    { zone = Time.utc
    , time = Time.millisToPosix 0
    }



--UPDATE--


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
                    changeCampusColor model.campus ( x, y ) color
                , history =
                    addHistory model.history ( x, y ) model.campus
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
                | subPalette =
                    addColorToSubPalette model.subPalette color
                , mainPalette =
                    model.colorValue
              }
            , Cmd.none
            )

        SetMainPalette n ->
            ( { model
                | mainPalette =
                    getSubPaletteColor model.subPalette n
              }
            , Cmd.none
            )

        DeleteSubPalette n ->
            ( { model
                | subPalette =
                    deleteSubPaletteColor model.subPalette n
                , mainPalette =
                    "white"
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
                toIntTemp : String -> Int
                toIntTemp str =
                    Maybe.withDefault 0 (String.toInt str)
            in
            ( { model
                | campus =
                    createNewCampus
                        ( model.tempCampusSize.width
                        , model.tempCampusSize.height
                        )
                , campusSize =
                    { width =
                        toIntTemp model.tempCampusSize.width
                    , height =
                        toIntTemp model.tempCampusSize.height
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
                    { tempSetting_
                        | borderColor =
                            if String.isEmpty value then
                                model.setting.borderColor

                            else
                                String.toLower value
                    }
              }
            , Cmd.none
            )

        SelectBorderStyle style ->
            ( { model
                | tempSetting =
                    { tempSetting_
                        | borderStyle = style
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
            ( { model
                | campus =
                    undoAndUpdateCampus model.history ( x, y ) model.campus
                , history =
                    Dict.remove (Dict.size model.history - 1) model.history
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
                | loadedSavedata =
                    decodeSavedata savedata
              }
            , applySavedata model
            )

        ApplySavedata model_ ->
            let
                savedata =
                    model.loadedSavedata
            in
            if isSavedata model.loadedSavedata then
                ( { model
                    | setting = decodeSetting savedata
                    , campus = decodeCampus savedata
                    , mainPalette = decodeMainPalette savedata
                    , campusSize = decodeCampusSize savedata
                    , didCreateCampus = decodeDidCreateCampus savedata
                    , subPalette = decodeSubPalette savedata
                    , history = decodeHistory savedata
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


applySavedata : Model -> Cmd Msg
applySavedata model =
    Task.perform ApplySavedata (Task.succeed model)


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



--VIEW--


view : Model -> Html Msg
view model =
    div
        [ HAttrs.style "height" "100%"
        ]
        [ layout
            []
          <|
            column [ E.width E.fill, E.height E.fill ]
                [ E.row
                    [ E.width E.fill
                    , E.height <| px 50
                    , Background.color <| rouIro
                    , paddingXY 16 0
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

                {- Horizontal Separator -}
                , E.el [ E.height <| px 1, E.width E.fill, Border.widthEach { top = 1, right = 0, left = 0, bottom = 0 }, Border.color shiroIro ] <| E.none
                , viewPanels model
                ]
        , loadCssElmReactor <| "../style.css"
        , createCampusModalWindow model
        , viewSaveModalWindow model
        , viewConfirmSaveCampusModalWindow model
        , viewSaveAndNewCampusModalWindow model
        ]


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


deleteSubPaletteColor : SubPalette -> Int -> SubPalette
deleteSubPaletteColor subPalette n =
    Dict.union
        (subPalette
            |> Dict.toList
            |> List.take (n - 1)
            |> Dict.fromList
        )
        (subPalette
            |> Dict.toList
            |> List.drop n
            |> Dict.fromList
            |> DictEx.mapKeys (\m -> m - 1)
        )


undoAndUpdateCampus : History -> Point -> Campus -> Campus
undoAndUpdateCampus history ( x_, y_ ) campus =
    let
        getLatestHistoryColor =
            history
                |> Dict.get (Dict.size history - 1)
                |> Maybe.withDefault ( ( 0, 0 ), "white" )
                |> Tuple.second
    in
    Dict.update
        ( x_, y_ )
        (Maybe.map (\n -> getLatestHistoryColor))
        campus


addHistory : History -> Point -> Campus -> History
addHistory history ( x, y ) campus =
    if Dict.size history <= 100 then
        Dict.insert
            (Dict.size history)
            ( ( x, y )
            , getCampusColor campus ( x, y )
            )
            history

    else
        let
            tempHistory =
                history
                    |> Dict.toList
                    |> List.drop 1
                    |> Dict.fromList
                    |> DictEx.mapKeys (\n -> n - 1)
        in
        Dict.insert
            (Dict.size tempHistory)
            ( ( x, y )
            , getCampusColor campus ( x, y )
            )
            tempHistory


createNewCampus : ( String, String ) -> Campus
createNewCampus ( widthS, heightS ) =
    let
        width =
            Maybe.withDefault 0 (String.toInt widthS)

        height =
            Maybe.withDefault 0 (String.toInt heightS)
    in
    Dict.fromList <|
        ListEx.lift2
            Tuple.pair
            (ListEx.lift2
                Tuple.pair
                (List.range 0 width)
                (List.range 0 height)
            )
            [ "white" ]


changeCampusColor : Campus -> Point -> CssColor -> Campus
changeCampusColor campus ( x, y ) color =
    Dict.update
        ( x, y )
        (Maybe.map (\n -> color))
        campus



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
