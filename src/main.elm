port module Main exposing (..)
import Browser
import Html as H exposing (..)
import Html.Attributes as HAttrs exposing(..)
import Html.Events as HEvents exposing (..)
import Html.Lazy as HLazy exposing (..)
import Array exposing (..)
import Dict exposing (..)
--import Debug exposing (..)
import Svg exposing (..)
import Process exposing (..)
import Task exposing (..)
import Regex exposing (..)
import List.Extra as ExList exposing (..)
import Result.Extra as ExResult exposing  (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Region as Region
import Bootstrap.Button as BBtn
import Bootstrap.Modal as BModal
import Bootstrap.CDN as BCDN
import Bootstrap.Grid as BGrid
import Bootstrap.Grid.Col as BCol
import Bootstrap.Grid.Row as BRow
import Bootstrap.Form.Input as BInput
import Json.Decode as JD
css path =
  H.node "link" [rel "stylesheet", href path] []

onChangeH : (String -> msg) -> H.Attribute msg
onChangeH handler =
    on "change" (JD.map handler HEvents.targetValue)

--MODEL--
type alias Model = 
    { campus : Campus
    , colorValue : CssColor
    , subPalette : SubPalette
    , mainPalette : CssColor
    , campusSize : CampusSize
    , tempCampusSize : TempCampusSize
    , modalVisibility : BModal.Visibility
    , openingModalWindow : BModal.Visibility
    , setting : Setting
    , tempSetting : Setting
    , borderColorValue : CssColor
    , toolsSetting : ToolsSetting
    , history : History
    }

type alias Campus =
    Dict Point CssColor 
type alias Point =
    (Int, Int)    
type alias Points =
    (Int, Int)
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
    Dict.fromList [((0, 0), "white")]
type alias TempCampusSize =
    { width : String
    , height : String
    }

type alias CampusSize =
    { width : Int
    , height : Int
    }

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
    Dict.fromList [(0, "white")]
type alias History =
    Dict Serial ((Point), CssColor)
initHistory : History
initHistory =
    Dict.fromList <| ExList.lift2 Tuple.pair (List.range 0 0) (ExList.lift2 Tuple.pair (ExList.lift2 Tuple.pair (List.range 0 0) (List.range 0 0)) ["white"])
    
type alias Setting =
    { borderColor : String
    , borderStyle : String
    , width : String
    , height : String
    , panelPosition : PanelPosition
    }

type alias PanelPosition =
    { settingPanel : Position
    , palettePanel : Position
    }
initPanelPosition : PanelPosition
initPanelPosition =
    { settingPanel = Left
    , palettePanel = Right
    }
initSetting : Setting
initSetting =
    { borderColor = "black"
    , borderStyle = "solid 1px"
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

--INIT--
init : () -> (Model, Cmd Msg)
init _ =
    ( { campus = initCampus
      , colorValue = initColorValue
      , subPalette = initSubPalette
      , mainPalette = initMainPalette
      , campusSize = (CampusSize 0 0)
      , tempCampusSize = (TempCampusSize "" "")
      , modalVisibility = BModal.hidden
      , openingModalWindow = BModal.shown
      , setting = initSetting
      , tempSetting = initSetting
      , borderColorValue = initBorderColorValue
      , toolsSetting = initToolsSetting
      , history = initHistory
      } 
    , Cmd.none
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
    | ForDisabled
    | ShowModal
    | CloseModal
    | BorderColorValue String
    | Change String
    | ChangePixelSize String String
    | SetPixelWidth String
    | SetPixelHeight String
    | CreateCampusPicture
    | DisplayDlButton
    | ChangePanelPosition Panel Position
    | ApplySetting
    | Undo Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeColor (x, y) color ->
            ( { model | campus = Dict.update 
                                    (x, y) 
                                        (Maybe.map (\n -> color))
                                            model.campus
                      , history = Dict.insert 
                                      (Dict.size model.history)
                                          ( (x, y)
                                          , getCampusColor model (x, y)
                                          )  
                                                                        model.history
              }
            , Cmd.none
            )

        ColorValue value ->
            ( { model | colorValue = if String.isEmpty model.colorValue then
                                        "white"
                                     else
                                        String.toLower value
              }
            , Cmd.none
            )

        AddColorToSubPalette color ->
            ( { model | subPalette = addColorToSubPalette model color
              ,         mainPalette = model.colorValue
              }
            , Cmd.none
            )

        SetMainPalette n ->
            ( { model | mainPalette = getPaletteColor model n 
              }
            , Cmd.none
            )
       
        DeleteSubPalette n ->
            ( { model | subPalette = Dict.remove n model.subPalette
                      , mainPalette = "white"
              }
            , Cmd.none
            )

        SetCampusWidth width ->
            ( { model | tempCampusSize = { width = width
                                         , height = model.tempCampusSize.height 
                                         } 
              }
            , Cmd.none
            )
        
        SetCampusHeight height ->
            ( { model | tempCampusSize = { height = height
                                         , width = model.tempCampusSize.width 
                                         } 
              }
            , Cmd.none
            )
       
        CreateCampus ->
            let
                createCampusList : Points-> List ( ( Int, Int ), String )
                createCampusList (width_, height_) =
                    ExList.lift2 
                        Tuple.pair ( ExList.lift2 
                                       Tuple.pair (List.range 0 width_) 
                                                  (List.range 0 height_)
                                   ) 
                                   ["white"]
                
                convertTemp : String -> Int
                convertTemp str =
                    Maybe.withDefault 0 (String.toInt str)
            in
                ( { model | campus = Dict.fromList <| 
                                        createCampusList ( convertTemp model.tempCampusSize.width
                                                         , convertTemp model.tempCampusSize.height
                                                         )
                          , campusSize = { width = convertTemp model.tempCampusSize.width
                                         , height = convertTemp model.tempCampusSize.height
                                         }
                          , openingModalWindow = BModal.hidden
                  }
                , Cmd.none
                )

        ForDisabled -> 
            ( model, Cmd.none )

        ShowModal ->
            ( { model | modalVisibility = BModal.shown }
            , Cmd.none
            )

        CloseModal ->
            ( { model | openingModalWindow = BModal.hidden }
            , Cmd.none
            )

        BorderColorValue value ->
            ( { model | tempSetting = { borderColor = if String.isEmpty value then
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
            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                      , borderStyle = str
                                      , width = model.tempSetting.width
                                      , height = model.tempSetting.height
                                      , panelPosition = model.tempSetting.panelPosition
                                      }
              }
            , Cmd.none
            )

        ChangePixelSize width_ height_ ->
            if (Maybe.withDefault 0 (String.toInt width_)) * (Maybe.withDefault 0 (String.toInt height_)) > 0 then
                ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                          , borderStyle = model.tempSetting.borderStyle
                                          , width = width_
                                          , height = height_
                                          , panelPosition = model.tempSetting.panelPosition
                                          } 
                  } 
                , Cmd.none
                )
            else
                (model, Cmd.none)

        SetPixelWidth tempWidth_ ->
            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                      , borderStyle = model.tempSetting.borderStyle
                                      , width = if String.isEmpty tempWidth_ then
                                                    model.setting.width
                                                else
                                                    tempWidth_
                                      , height = model.tempSetting.height
                                      , panelPosition = model.tempSetting.panelPosition
                                      }
              }
            , Cmd.none
            )
        SetPixelHeight tempHeight_ ->
            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                      , borderStyle = model.tempSetting.borderStyle
                                      , width = model.tempSetting.width
                                      , height = if String.isEmpty tempHeight_ then
                                                    model.setting.height
                                                else
                                                    tempHeight_
                                      , panelPosition = model.tempSetting.panelPosition
                                      }
              }
            , Cmd.none
            )

        CreateCampusPicture ->
            ( model, toH2c () )

        DisplayDlButton ->
            ( { model | toolsSetting = { isDisplayDlButton = True
                                       } 
              }
            , toH2c ()
            )
        
        ChangePanelPosition panel_ position_ ->
            case panel_ of
                SettingPanel ->
                    case position_ of
                        Right ->
                            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                                      , borderStyle = model.tempSetting.borderStyle
                                                      , width = model.tempSetting.width
                                                      , height = model.tempSetting.height
                                                      , panelPosition =  { settingPanel = Right
                                                                         , palettePanel = model.tempSetting.panelPosition.palettePanel
                                                                         }
                                                      } 
                              } 
                            , Cmd.none
                            )
                        Left ->
                            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                                      , borderStyle = model.tempSetting.borderStyle
                                                      , width = model.tempSetting.width
                                                      , height = model.tempSetting.height
                                                      , panelPosition =  { settingPanel = Left
                                                                         , palettePanel = model.tempSetting.panelPosition.palettePanel
                                                                         }
                                                      } 
                              } 
                            , Cmd.none
                            )
                PalettePanel ->
                    case position_ of
                        Right ->
                            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                                      , borderStyle = model.tempSetting.borderStyle
                                                      , width = model.tempSetting.width
                                                      , height = model.tempSetting.height
                                                      , panelPosition =  { settingPanel = model.tempSetting.panelPosition.settingPanel
                                                                         , palettePanel = Right
                                                                         }
                                                      } 
                              } 
                            , Cmd.none
                            )
                        Left ->
                            ( { model | tempSetting = { borderColor = model.tempSetting.borderColor
                                                      , borderStyle = model.tempSetting.borderStyle
                                                      , width = model.tempSetting.width
                                                      , height = model.tempSetting.height
                                                      , panelPosition =  { settingPanel = model.tempSetting.panelPosition.settingPanel
                                                                         , palettePanel = Left
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
                (model, Cmd.none)

        Undo (y, x) ->
            let
                getHistoryColor : Point -> String 
                getHistoryColor (x_, y_) =
                    Tuple.second <| 
                        Maybe.withDefault ( (0, 0), "white" ) <|
                            Dict.get ((Dict.size model.history) - 1) model.history
            in
            ( { model | campus = Dict.update 
                                    (x, y) 
                                        (Maybe.map (\n -> getHistoryColor (x, y) )) 
                                            model.campus
                      , history = Dict.remove ((Dict.size model.history) - 1) model.history
              }
            , toClickJudge ()
            ) 

--VIEW--
view : Model -> Html Msg
view model =
    div [ HAttrs.style "height" "100%"
        ]
        [ css <| "../style.css"
        , createCampusWindow model
        , layout [debugLine False
                 ] <|
            column [ E.width fill, E.height fill, debugLine False]
                [ row [ debugLine False
                      , E.width fill
                      , E.height <| px 50
                      , Background.color <| rouIro
                      , paddingXY 15 0
                      , Border.widthEach { top = 1, right = 1, left = 1, bottom = 0 }
                      , Border.color <| shiroIro
                      ]
                      [ E.el [alignLeft
                             , Font.color <| shiroIro
                             ] <| 
                                E.text "SOSOGU"
                      , row [ alignRight 
                            , spacing 6
                            ]
                            [ newTabLink []
                                         { url = ""
                                         , label = E.row [ spacing 3 ]
                                                         [ E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)" ]
                                                                   { src = "file/home.svg"
                                                                   , description = ""
                                                                   }
                                                         , E.el [ Font.color <| shiroIro 
                                                                , Font.size <| 16
                                                                ] <|
                                                            E.text "Home"
                                                         ]
                                         }
                            , newTabLink []
                                         { url = "https://github.com/floyd33n/SOSOGU"
                                         , label = E.row [ spacing 3 ]
                                                         [ E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)" ]
                                                                   { src = "file/mark-github.svg"
                                                                   , description = ""
                                                                   }
                                                         , E.el [ Font.color <| shiroIro
                                                                , Font.size <| 16
                                                                ] <|
                                                            E.text "Repository"
                                                         ]
                                         }
                            ] 
                      ]
                , row [ E.width fill
                      , E.height fill
                      , debugLine False 
                      ] 
                      [ settingPosition model (model.setting.panelPosition.settingPanel == Left)
                      , palettePosition model (model.setting.panelPosition.palettePanel == Left)
                      , column [ E.width fill
                               , E.height fill
                               , Background.color <| shironezuIro
                               ] 
                               [ toolsPanel model True
                               , el [centerX] <| 
                                  html <|
                                      viewCampus model (model.campusSize.width, model.campusSize.height)
                               ]  
                      , palettePosition model (model.setting.panelPosition.palettePanel == Right)
                      , settingPosition model (model.setting.panelPosition.settingPanel == Right)
                      ]
                ]
        ]

toolsPanel : Model -> Bool -> Element Msg
toolsPanel model bool =
    let
        gendlButton : String -> Element Msg
        gendlButton bText =
             let
                 tempButton : Maybe Msg -> String -> String -> String -> H.Attribute Msg -> H.Attribute Msg -> Element Msg
                 tempButton msg oValue dValue id_ attr1 attr2 =
                     Input.button []
                                  { onPress = msg
                                  , label = E.el [] <|
                                      html <|
                                         H.a [ HAttrs.style "color" "white"
                                             , HAttrs.style "font-size" "14px"
                                             , HAttrs.style "opacity" oValue
                                             , HAttrs.style "text-decoration" dValue
                                             , id id_
                                             , attr1
                                             , attr2
                                             ]
                                             [ H.text bText ]
                                  }
             in
                case bText of
                    "Gen" ->
                        if (Dict.size model.campus > 1) then
                            tempButton (Just DisplayDlButton) "1" "none" "" (HAttrs.style "" "") (hidden False)
                        else
                            tempButton Nothing "0.6" "line-through" "" (HAttrs.style "" "") (hidden False)
                    "DL" ->
                        if model.toolsSetting.isDisplayDlButton then
                            tempButton Nothing "1" "none" "dl" (href "") (target "_blank")
                        else
                            tempButton Nothing "0.6" "line-through" "" (HAttrs.style "" "") (hidden False)
                    _ ->
                        E.none
        viewUndoButton : Element Msg
        viewUndoButton =
            Input.button []
                         { onPress =
                             let
                                 x =  Tuple.first (Tuple.first (Maybe.withDefault ((0, 0), "white") (Dict.get ((Dict.size model.history)-1) model.history)))
                                 y =  Tuple.second (Tuple.first (Maybe.withDefault ((0, 0), "white") (Dict.get ((Dict.size model.history)-1) model.history)))
                             in
                                 Just <| Undo (y, x)
                         , label = E.el [ Font.color <| shiroIro
                                        , Font.size <| 14
                                        ] <|
                                            E.text <| "Undo"
                         }
    in
    if bool then
        row [ E.width fill
            , E.height <| px 36
            , Border.widthEach { top = 1, right = 0, left = 0, bottom = 1 }
            , Border.color <| shiroIro
            , Background.color <| rouIro
            ]
            [ E.el [ Font.color <| shiroIro
                   , Font.size <| 17
                   , padding 2
                   , centerY
                   ] <|
                      E.text "Tools"
            , E.row [ alignRight
                    , paddingXY 20 0
                    , spacing 5
                    ]
                    [ gendlButton "Gen"
                    , gendlButton "DL"
                    , viewUndoButton
                    ]
            ]
    else
        E.none


changePositionText : Position -> String
changePositionText position =
    case position of
        Right ->
            "To Left"
        Left ->
            "To Right "

panelHr : Element Msg
panelHr =
    E.el [ centerX
         , E.width <| px 80
         , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
         , Border.color <| shiroIro
         ] <|
         none

palettePosition : Model -> Bool -> Element Msg
palettePosition model bool  =
    if bool then
        column [ E.width <| px 100
               , E.height fill
               , Border.width 1
               , Border.color<| shiroIro
               , Background.color <| rouIro
               , debugLine False
               ]
               [ el [ Font.color <| shiroIro
                    , Font.size <| 17
                    , centerX
                    , padding 2
                    ] <| 
                        E.text "Palette"
               , panelHr
               , column [ centerX
                        , padding 3
                        , spacing 5
                        ] 
                        [ E.el [ Font.color <| shiroIro
                               , Font.size <| 14
                               , centerX
                               ]  <|
                                   E.text "Add Color"
                        , panelHr
                        , E.el [ E.width <| px 90
                               ] <| 
                                   html <|
                                       H.input [ onInput ColorValue 
                                               , HAttrs.style "width" "80px"
                                               , HAttrs.style "height" "14px"
                                               , HAttrs.style "font-size" "0.7em"
                                               , HAttrs.style "margin" "0 auto"
                                               ] 
                                               [H.text model.colorValue]
                        , el [centerX] <|
                            html <|
                                div [ HAttrs.style "color" "#e2041b" 
                                    , HAttrs.style "font-size" "13px"
                                    ]
                                    [ H.text <| if isColor model.colorValue then
                                                    ""
                                                else
                                                    if String.isEmpty model.colorValue then
                                                        "Is Empty"
                                                    else
                                                        "Isn't Color"
                                    ]
                        , el [ centerX ] <|
                              if isColor <| model.colorValue then
                                  Input.button [ htmlAttribute <| HAttrs.style "color" "white"
                                               ] 
                                               { onPress = Just (AddColorToSubPalette model.colorValue)
                                               , label = row []
                                                             [ E.el [ Font.color <| shiroIro
                                                                    , Font.size <| 14
                                                                    ] <|
                                                                        E.text "Add "
                                                             , html <|
                                                                div [ HAttrs.style "width" "14px"
                                                                    , HAttrs.style "height" "14px"
                                                                    , HAttrs.style "background-color" model.colorValue
                                                                    ]
                                                                    []
                                                             ] 

                                               } 
                              else
                                  E.el [ htmlAttribute <| HAttrs.style "opacity" "0.6"] <|
                                      Input.button [ Region.description "disabled"
                                                   , htmlAttribute <| HAttrs.style "color" "white"
                                                   ] 
                                                   { onPress = Just ForDisabled
                                                   , label = E.el [ Font.color <| shiroIro
                                                                  , Font.size <| 14
                                                                  ] <| 
                                                                      E.text "disabled"
                                                    }
                        ]
               , column [ centerX
                        , padding 3
                        , spacing 5
                        ]
                        [ E.el [ Font.color <| shiroIro
                               , Font.size <| 14
                               , centerX
                               ]  <|
                                   E.text "Main Palette"
                        , panelHr
                        , E.el [ centerX
                               ] <|
                                   html <|
                                       div [ HAttrs.style "width" "30px"
                                           , HAttrs.style "height" "30px"
                                           , HAttrs.style "border" "solid 1px black"
                                           , HAttrs.style "background-color" model.mainPalette 
                                           ] 
                                           [] 
                        ]
               , column [ centerX
                        , paddingEach { top = 2, right = 0, left = 0, bottom = 2 }
                        , spacing 5
                        ]
                        [ E.el [ Font.color <| shiroIro
                               , Font.size <| 14
                               , centerX
                               ] <|
                                  E.text "Sub Palette"
                        , panelHr
                        , column [ centerX
                                ]
                                [ wrappedRow [ spacing 3 ] <|
                                    List.map (\plt -> E.el [] <|
                                                          html <|
                                                              div [ HAttrs.style "width" "25px"
                                                                  , HAttrs.style "height" "25px"
                                                                  , HAttrs.style "background-color" <| getPaletteColor model (plt-1)
                                                                  , HAttrs.style "border" "solid 1px black"
                                                                  , onDoubleClick (DeleteSubPalette plt)
                                                                  , onClick (SetMainPalette (plt-1))
                                                                  ]  
                                                                  []
                                             ) <|
                                                List.range 1 (Dict.size model.subPalette)
                                ]
                        ]
               ]
    else
        E.none

settingPosition : Model -> Bool -> Element Msg
settingPosition model bool  =
    if bool then
        column [ E.width <| px 100
               , E.height fill
               , Border.width 1
               , Border.color <| shiroIro
               , Background.color <| rouIro
               ]
               [ el [ Font.color <| shiroIro
                    , Font.size <| 17
                    , centerX
                    , padding 2
                    ] <| 
                        E.text "Setting"
              , panelHr
              -- Border Color --
              , column [ centerX
                       , spacing 5
                       , padding 3
                       ] 
                       [ E.el [ Font.size 14
                              , Font.color <| shiroIro
                              , centerX
                              ] <|
                                  E.text "Border Color"
                       , panelHr
                       , E.el [ E.width <| px 90 
                              ] <|
                                  html <|
                                      H.input [ onInput BorderColorValue
                                              , HAttrs.style "width" "80px"
                                              , HAttrs.style "height" "14px"
                                              , HAttrs.style "font-size" "0.7em"
                                              , HAttrs.placeholder "black"
                                              , HAttrs.style "margin" "0 auto"
                                              ]  
                                              [ H.text model.tempSetting.borderColor ]
                       , el [centerX] <|
                           html <|
                               div [ HAttrs.style "color" "#e2041b" 
                                   , HAttrs.style "font-size" "13px"
                                   ]
                                   [ H.text <| if isColor model.tempSetting.borderColor then
                                                   ""
                                               else
                                                   if String.isEmpty model.tempSetting.borderColor then
                                                       "Is Empty"
                                                   else
                                                       "Isn't Color"
                                   ]
                       ]
              -- Border Style --
              , column [ centerX
                       , padding 3
                       , spacing 5
                       ]
                       [ E.el [ Font.size 14 
                              , Font.color <| shiroIro
                              ] <| 
                          E.text "Border Style"
                       , panelHr
                       , E.el [centerX, E.height <| px 14
                              , Font.size <| 14
                              ] <|
                                  html <|
                                      let
                                          handler selectedValue =
                                              Change selectedValue

                                          option_ : String -> String -> Html Msg
                                          option_ value_ text_ =
                                              option [ value value_ ] 
                                                     [ H.text text_ ]
                                      in
                                          div [ HAttrs.style "height" "14px"
                                              , HAttrs.style "font-size" "14px" 
                                              ]
                                              [ H.select [ onChangeH handler ]
                                                         [ option_ "solid 1px" "solid"
                                                         , option_ "none" "none"
                                                         , option_ "double" "double"
                                                         , option_ "groove" "groove"
                                                         , option_ "ridge" "ridge"
                                                         , option_ "dashed 1px" "dashed"
                                                         , option_ "dotted 1px" "dotted"
                                                         ]
                                              ]
                       ]
              -- Pixel Size --
              , column [ centerX
                       , padding 3
                       , spacing 5
                       ] 
                       [ E.el [ Font.size 14
                              , Font.color <| shiroIro
                              , centerX
                              ] <|
                                  E.text "Pixel Size"
                       , panelHr
                       , column [ spacing 3 ] <|
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
                                [ html <|
                                    div [ HAttrs.style "color" "white" 
                                        , HAttrs.style "font-size" "14px"
                                        , HAttrs.style "padding" "3px"
                                        ]
                                        [ H.text "width : "
                                        , H.input [ HAttrs.style "" ""
                                                  , HAttrs.style "width" "30px"
                                                  , HAttrs.style "height" "14px"
                                                  , HAttrs.style "font-size" "95%"
                                                  , placeholder model.setting.width
                                                  , onInput SetPixelWidth
                                                  ]  
                                                  [ H.text model.tempSetting.width ]
                                        , div [ HAttrs.style "color" "#e2041b"
                                              , HAttrs.style "font-size" "13px"
                                              ] 
                                              [ H.text <| pixelSizeErr model.tempSetting.width ]
                                        ]
                                , html <|
                                    div [ HAttrs.style "color" "white" 
                                        , HAttrs.style "font-size" "14px"
                                        , HAttrs.style "padding" "3px"
                                        ]
                                        [ H.text "height : "
                                        , H.input [ HAttrs.style "width" "30px" 
                                                  , HAttrs.style "height" "14px"
                                                  , HAttrs.style "font-size" "95%"
                                                  , placeholder model.setting.height
                                                  , onInput SetPixelHeight
                                                  ]
                                                  []
                                        , div [ HAttrs.style "color" "#e2041b"
                                              , HAttrs.style "font-size" "13px"
                                              ] 
                                              [ H.text <| pixelSizeErr model.tempSetting.height ]
                                        ]
                                ]
                       ]
              --panel position--
              , column [ centerX
                       , padding 3
                       , spacing 5
                       ]
                       [ E.el [ Font.size 14
                              , Font.color <| shiroIro
                              , centerX
                              ] <|
                                  E.text "Position"
                       , panelHr
                       , E.el [centerX] <|
                          html <|
                              div []
                                  [ div [ HAttrs.style "color" "white"
                                        , HAttrs.style "font-size" "14px"
                                        ]
                                        [ H.text "Setting"
                                        , br [] []
                                        , H.text "R"
                                        , H.input [ type_ "radio"
                                                  , value "right"
                                                  , name "settingpanel"
                                                  , onClick <| ChangePanelPosition SettingPanel Right
                                                  , checked <| model.tempSetting.panelPosition.settingPanel == Right
                                                  ]
                                                  []
                                        , H.text "L"
                                        , H.input [ type_ "radio"
                                                  , value ""
                                                  , name "settingpanel"
                                                  , onClick <| ChangePanelPosition SettingPanel Left
                                                  , checked <| model.tempSetting.panelPosition.settingPanel == Left
                                                  ]
                                                  []
                                        ]
                                  , div [ HAttrs.style "color" "white"
                                        , HAttrs.style "font-size" "14px"
                                        ] 
                                        [ H.text "Palette"
                                        , br [] []
                                        , H.text "R"
                                        , H.input [ type_ "radio"
                                                  , value ""
                                                  , name "paletteposition"
                                                  , onClick <| ChangePanelPosition PalettePanel Right
                                                  , checked <| model.tempSetting.panelPosition.palettePanel == Right
                                                  ]
                                                  []
                                        , H.text "L"
                                        , H.input [ type_ "radio"
                                                  , value ""
                                                  , name "paletteposition"
                                                  , onClick <| ChangePanelPosition PalettePanel Left
                                                  , checked <| model.tempSetting.panelPosition.palettePanel == Left
                                                  ]
                                                  []
                                        ] 
                                  ]
                       ]
              , E.el [centerX] <|
                  if isCorrectSetting model.tempSetting then
                      Input.button [ htmlAttribute <| HAttrs.style "color" "white"
                                   ]
                                   { onPress = Just ApplySetting
                                   , label = E.el [ Font.color <| shiroIro
                                                  , Font.size <| 14
                                                  ] <|
                                                      E.text "Apply"
                                   }
                  else
                      Input.button [ htmlAttribute <| HAttrs.style "opacity" "0.6" 
                                   , htmlAttribute <| HAttrs.style "color" "white"
                                   ]
                                   { onPress = Nothing
                                   , label = E.el [ Font.color <| shiroIro
                                                  , Font.size <| 14
                                                  ] <| 
                                                      E.text "disabled"
                                   }
              ]
              
    else
        E.none

isCorrectSetting : Setting -> Bool
isCorrectSetting setting =
    (isColor setting.borderColor) && (isCorrectWidthHeight setting.width setting.height)

createCampusWindow : Model -> Html Msg
createCampusWindow model =
    BGrid.container [] 
                    [ BModal.config CloseModal
                        |> BModal.hideOnBackdropClick False
                        |> BModal.small
                        |> BModal.h5 [ HAttrs.style "margin" "0 auto"
                                     ] 
                                     [ H.text "Enter Campus Size" ]
                        |> BModal.body []
                                       [ BGrid.containerFluid []
                                                              [ BGrid.row []
                                                                          [ BGrid.col [ BCol.xs6 ]
                                                                                      [ div [HAttrs.style "margin" "0 auto"] 
                                                                                            [H.text "Width"]
                                                                                      , BInput.number [ BInput.small
                                                                                                      , BInput.onInput SetCampusWidth
                                                                                                      ]
                                                                                      ] 
                                                                          , BGrid.col [ BCol.xs5 ]
                                                                                      [ div [HAttrs.style "margin" "0 auto"] 
                                                                                            [H.text "Height"]
                                                                          , BInput.number [ BInput.small
                                                                                          , BInput.onInput SetCampusHeight
                                                                                          ]
                                                                                      ]
                                                                          ]
                                                              ]
                                       ]
                        |> BModal.footer [HAttrs.style "margin" "auto"]
                                         [ BBtn.button [ BBtn.outlinePrimary
                                                       , if (isCorrectWidthHeight model.tempCampusSize.width model.tempCampusSize.height) then BBtn.primary else BBtn.secondary
                                                       , BBtn.attrs [ onClick CreateCampus ]
                                                       , BBtn.disabled <| (not (isCorrectWidthHeight model.tempCampusSize.width model.tempCampusSize.height))
                                                       ]
                                                       [ H.text "Create!" ]
                                         ]                                             
                        |> BModal.view model.openingModalWindow
                    ]
--DEBUG--
debugLine : Bool ->  E.Attribute Msg
debugLine bool =
    if bool then
        explain Debug.todo
    else
        htmlAttribute <| HAttrs.style "" ""
--FUNC--
getPaletteColor : Model -> Int -> String
getPaletteColor model n =
    Maybe.withDefault "white" (Dict.get n model.subPalette)

getCampusColor : Model -> Point -> String
getCampusColor model (x, y) =
    Maybe.withDefault "white" (Dict.get (x, y) model.campus)

viewCampus : Model -> Points -> Html Msg
viewCampus model (width, height) =
    div [ id "campus" ]
        [ div [] <|
            List.map (\y -> div [] <|
                List.map ( \x -> div [ HAttrs.style "float" "left"
                                     ] <|
                    [ div [ HAttrs.style "width" (model.setting.width ++ "px")
                          , HAttrs.style "height" (model.setting.height ++ "px")
                          , HAttrs.style "border" (model.setting.borderColor ++ " " ++ model.setting.borderStyle)
                          , HAttrs.style "background-color" (getCampusColor model (y, x) )
                          , HAttrs.style "padding" "0px"
                          , HAttrs.style "margin" "-1px"
                          , HEvents.onClick (ChangeColor (y, x) model.mainPalette)
                          --, HEvents.onDoubleClick (ChangeColor y x "white")
                          ]
                          []
                    ]
                         ) <|
                            List.range 0 (width-1)
                      ) <|
                          List.range 0 (height-1)
        ]

addColorToSubPalette : Model -> CssColor -> SubPalette
addColorToSubPalette model color =
    Dict.insert (Dict.size model.subPalette) color model.subPalette

displayPalette : Model -> Html Msg
displayPalette model =
    div [] <| 
        List.map ( \plt -> div []
                               [ div [ id "palette_square"
                                     , HEvents.onClick <| SetMainPalette (plt - 1)
                                     , HAttrs.style "background-color" <| getPaletteColor model (plt - 1)
                                     ] 
                                     []
                               , div [ id "palette_color_name" ] 
                                     []
                               ]
                 ) <| 
                     List.range 1 (Dict.size model.subPalette)

isColor : String -> Bool
isColor exValue =
    case (String.left 1 exValue) of
        "#" ->
            xor 
                ( Regex.contains (Maybe.withDefault  Regex.never <| Regex.fromString "[g-z]" ) 
                                 (String.dropLeft 1 exValue)
                ) <| 
                String.length exValue == 4 || String.length exValue == 7

        _ ->
            let 
                cssColorNames = ["aliceblue", "antiquewhite", "aqua", "aquamarine", "azure", "beige", "bisque", "black", "blanchedalmond", "blue"
                                , "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", "crimson"
                                , "cyan", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgrey", "darkgreen", "darkkhaki", "darkmagenta", "darkolivegreen"
                                , "darkorange", "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategrey", "darkslategray", "darkturquoise", "darkviolet"
                                , "deeppink", "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", "forestgreen", "fuchsia", "gainsboro"
                                , "ghostwhite", "gold", "goldenrod", "gray", "grey", "green", "greenyellow", "honeydew", "hotpink", "indianred"
                                , "indigo", "ivory", "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan"
                                , "lightgoldenrodyellow", "lightgray", "lightgrey", "lightgreen", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", "lightslategray", "lightslategrey"
                                , "lightsteelblue", "lightyellow", "lime", "limegreen", "linen", "magenta", "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple"
                                , "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin", "navajowhite"
                                , "navy", "oldlace", "olive", "olivedrab", "orange", "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise"
                                , "palevioletred", "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "purple", "rebeccapurple", "red"
                                , "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna", "silver", "skyblue"
                                , "slateblue", "slategray", "slategrey", "snow", "springgreen", "steelblue", "tan", "teal", "thistle", "tomato"
                                , "turquoise", "violet", "wheat", "white", "whitesmoke", "yellow", "yellowgreen"]
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
            * 
            Maybe.withDefault 0 (String.toInt height_) 
            > 0

        chkLength : Bool
        chkLength =
            Maybe.withDefault 0 (String.toInt width_) <= 64 
            && 
            Maybe.withDefault 0 (String.toInt height_) <= 64
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
    Sub.none

port toH2c : () -> Cmd msg
port toClickJudge : () -> Cmd msg
--port fromH2c -> 
--ColorSet--
rouIro = rgb255 43 43 43
sumiIro = rgb255 89 88 87
shiroIro = rgb255 255 255 255
shironeriIro = rgb255 243 243 242
shironezuIro = rgb255 220 221 221
