module Main exposing(main)
import Browser
import Html as H exposing (..)
import Html.Attributes as HAttrs exposing(..)
import Html.Events as HEvents exposing (..)
import Array exposing (..)
import Debug exposing (..)
import Svg exposing (..)
import Regex exposing (..)
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
import Json.Decode as Json
css path =
  H.node "link" [rel "stylesheet", href path] []

onChange : (String -> msg) -> H.Attribute msg
onChange handler =
    on "change" (Json.map handler HEvents.targetValue)

--MODEL--
type alias Model = 
    { campus : List (List (Int, String))
    , colorValue : String
    , palette : List String
    , mainPalette : String
    , campusSize : CampusSize
    , tempCampusSize : TempCampusSize
    , palettePosition : Position
    , settingPosition : Position
    , modalVisibility : BModal.Visibility
    , openingModalWindow : BModal.Visibility
    , campusSetting : CampusSetting
    , borderColorValue : String
    }

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

type alias CampusSetting =
    { borderColor : String
    , borderStyle : String
    , width : String
    , tempWidth : String
    , height : String
    , tempHeight : String
    }

initCampusSetting : CampusSetting
initCampusSetting =
    { borderColor = "black"
    , borderStyle = "solid 1px"
    , width = "10"
    , tempWidth = ""
    , height = "10"
    , tempHeight = ""
    }

--INIT--
init : () -> (Model, Cmd Msg)
init _ =
    ( { campus =  [[(0, "")]]
      , colorValue = ""
      , palette = []
      , mainPalette = "white"
      , campusSize = (CampusSize 0 0)
      , tempCampusSize = (TempCampusSize "" "")
      , palettePosition = Right
      , settingPosition = Left
      , modalVisibility = BModal.hidden
      , openingModalWindow = BModal.shown
      , campusSetting = initCampusSetting
      , borderColorValue = ""
      } 
    , Cmd.none
    )

--UPDATE--
type Msg
    = ChangeColor Int Int String
    | ColorValue String
    | AddColorToPalette String
    | SetMainPalette Int
    | SetCampusWidth String
    | SetCampusHeight String
    | CreateCampus
    | ForDisabled
    | ShowModal
    | CloseModal
    | ChangePosition Panel
    | BorderColorValue String
    | UpdateCampusSetting String
    | Change String
    | ChangePixelSize String String
    | SetPixelWidth String
    | SetPixelHeight String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeColor x y color ->
            ( { model | campus = updateCampus model x y color}
            , Cmd.none
            )

        ColorValue value ->
            ( { model | colorValue = (String.toLower value) }
            , Cmd.none
            )

        AddColorToPalette color ->
            ( { model | palette = addColorToPalette model color
              ,         mainPalette = model.colorValue
              }
            , Cmd.none
            )

        SetMainPalette n ->
            ( { model | mainPalette = getPaletteColor model n }
            , Cmd.none
            )
        
        SetCampusWidth width ->
            ( { model | tempCampusSize = { width = width, height = model.tempCampusSize.height } }
            , Cmd.none
            )
        
        SetCampusHeight height ->
            ( { model | tempCampusSize = { height = height, width = model.tempCampusSize.width } }
            , Cmd.none
            )
       
        CreateCampus ->
            let
                testFuncA : Model -> List (List (Int, String))
                testFuncA tmodel =
                    let
                        temp = { width = (Maybe.withDefault 0 (String.toInt tmodel.tempCampusSize.width)), height = (Maybe.withDefault 0 (String.toInt tmodel.tempCampusSize.height)) }
                    in
                        List.repeat temp.height (Array.toIndexedList (Array.fromList (List.repeat temp.width "white")))
            in
                ( { model | campus = testFuncA model
                          , campusSize = { width = (Maybe.withDefault 0 (String.toInt model.tempCampusSize.width)), height = (Maybe.withDefault 0 (String.toInt model.tempCampusSize.height)) }
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

        ChangePosition panel ->
            case panel of
                PalettePanel ->
                    ( { model | palettePosition =
                                    case model.palettePosition of
                                        Right ->
                                            Left
                                        Left ->
                                            Right
                      }
                    , Cmd.none
                    )
                SettingPanel ->    
                    ( { model | settingPosition =
                                    case model.settingPosition of
                                        Right ->
                                            Left
                                        Left ->
                                            Right
                      }
                    , Cmd.none
                    )
        
        BorderColorValue value ->
            ( { model | borderColorValue = (String.toLower value) }
            , Cmd.none
            )

        UpdateCampusSetting color ->
            ( { model | campusSetting = { borderColor = model.borderColorValue 
                                        , borderStyle = model.campusSetting.borderStyle  
                                        , width = model.campusSetting.width
                                        , tempWidth = model.campusSetting.tempWidth
                                        , height = model.campusSetting.height
                                        , tempHeight = model.campusSetting.tempHeight
                                        }
              }
            , Cmd.none
            )

        Change str ->
            ( { model | campusSetting = { borderColor = model.campusSetting.borderColor
                                        , borderStyle = str
                                        , width = model.campusSetting.width
                                        , tempWidth = model.campusSetting.tempWidth
                                        , height = model.campusSetting.height
                                        , tempHeight = model.campusSetting.tempHeight
                                        }
              }
            , Cmd.none
            )

        ChangePixelSize width_ height_ ->
            if (Maybe.withDefault 0 (String.toInt width_)) * (Maybe.withDefault 0 (String.toInt height_)) > 0 then
                ( { model | campusSetting = { borderColor = model.campusSetting.borderColor
                                            , borderStyle = model.campusSetting.borderStyle
                                            , width = width_
                                            , tempWidth = model.campusSetting.tempWidth
                                            , height = height_
                                            , tempHeight = model.campusSetting.tempHeight
                                            } 
                  } 
                , Cmd.none
                )
            else
                (model, Cmd.none)

        SetPixelWidth tempWidth_ ->
            ( { model | campusSetting = { borderColor = model.campusSetting.borderColor
                                        , borderStyle = model.campusSetting.borderStyle
                                        , width = model.campusSetting.width
                                        , tempWidth = tempWidth_
                                        , height = model.campusSetting.height
                                        , tempHeight = model.campusSetting.tempHeight
                                        }
              }
            , Cmd.none
            )
        SetPixelHeight tempHeight_ ->
            ( { model | campusSetting = { borderColor = model.campusSetting.borderColor
                                        , borderStyle = model.campusSetting.borderStyle
                                        , width = model.campusSetting.width
                                        , tempWidth = model.campusSetting.tempWidth
                                        , height = model.campusSetting.height
                                        , tempHeight = tempHeight_
                                        }
              }
            , Cmd.none
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
                      , Border.width 1
                      , Border.color <| rgb255 255 255 255
                      ]
                      [ E.el [alignLeft
                             , Font.color <| rgb255 255 255 255
                             ] <| 
                                E.text "SOSOGU"
                      , row [ alignRight 
                            , spacing 5
                            ]
                            [ E.el [] <|
                                E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)"
                                        ]
                                        { src = "svg/home.svg"
                                        , description = "Home"
                                        }
                            , E.el [ Font.color <| rgb255 255 255 255 
                                   , Font.size 16 
                                   ] <|
                                E.text "Home"
                            , E.el [] <|
                                E.image [ htmlAttribute <| HAttrs.style "filter" "invert(100%)" ]
                                        { src = "svg/mark-github.svg"
                                        , description = "GitHub"
                                        }
                            , E.el [ Font.color <| rgb255 255 255 255 
                                   , Font.size 16
                                   ] <|
                                E.text "GitHub"
                            ]
                      ]
                , row [ E.width fill
                      , E.height fill
                      , debugLine False 
                      ] 
                      [ settingPosition model (model.settingPosition == Left)
                      , palettePosition model (model.palettePosition == Left)
                      , column [ E.width fill
                               , E.height fill
                               , Background.color <| shironezuIro
                               ] 
                               [ E.text "campus"
                               , el [ centerX, centerY ] <| 
                                  html <| 
                                      makeTable model model.campusSize.width model.campusSize.height
                               ]  
                               , palettePosition model (model.palettePosition == Right)
                               , settingPosition model (model.settingPosition == Right)
                      ]
                ]
        ]

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
               [ el [ Font.color <| rgb255 255 255 255
                    , Font.size <| 17
                    , centerX
                    , padding 2
                    ] <| 
                        E.text "Palette"
               , panelHr
               {-
               -}
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
                        , el [ centerX ] <|
                              if isColor <| model.colorValue then
                                  Input.button [ htmlAttribute <| HAttrs.style "color" "white"
                                               ] 
                                               { onPress = Just (AddColorToPalette model.colorValue)
                                               , label = E.el [ Font.color <| shiroIro
                                                              , Font.size <| 14
                                                              ] <|
                                                                  E.text "Add"
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
                        , showSubPalette model
                        ]
               , Input.button [ alignBottom
                              , Font.color <| shiroIro
                              ]
                              { onPress = Just <| ChangePosition PalettePanel
                              , label = E.text <| changePositionText model.palettePosition
                              } 
               ]
    else
        E.none

showSubPalette : Model -> Element Msg
showSubPalette model =
    column [ centerX
           ]
           [ wrappedRow [spacing 3] <| List.map (\plt -> E.el [] <|
                                                          html <|
                                                              div [ HAttrs.style "width" "25px"
                                                                  , HAttrs.style "height" "25px"
                                                                  , HAttrs.style "background-color" <| getPaletteColor model (plt-1)
                                                                  , HAttrs.style "border" "solid 1px black"
                                                                  ] 
                                                                  []
                                                ) <| List.range 1 (List.length model.palette)
           ]

showPalette : Element Msg
showPalette =
    E.el [  
         ] <|
        html <|
            div [ HAttrs.style "width" "25px"
                , HAttrs.style "height" "25px"
                , HAttrs.style "background-color" "white"
                , HAttrs.style "border" "solid 1px black"
                ] 
                [] 

settingPosition : Model -> Bool -> Element Msg
settingPosition model bool  =
    if bool then
        column [ E.width <| px 100
               , E.height fill
               , Border.width 1
               , Border.color <| shiroIro
               , Background.color <| rouIro
               ]
               [ el [ Font.color <| rgb255 255 255 255
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
                                              [ H.text model.campusSetting.borderColor ]
                       , el [ centerX 
                            , E.height <| px 15
                            ] <|
                                if isColor<|model.borderColorValue then
                                    Input.button [ htmlAttribute <| HAttrs.style "color" "white"
                                                 ] 
                                                 { onPress = Just (UpdateCampusSetting model.colorValue)
                                                 , label = E.el [ Font.color <| shiroIro
                                                                , Font.size <| 14
                                                                ] <|
                                                                    E.text "Apply"
                                                 } 
                                else
                                    Input.button [ Region.description "disabled"
                                                 , htmlAttribute <| HAttrs.style "color" "white"
                                                 , htmlAttribute <| HAttrs.style "opacity" "0.6"
                                                 ] 
                                                 { onPress = Just ForDisabled
                                                 , label = E.el [ Font.color <| shiroIro
                                                                , Font.size <| 14
                                                                ] <| 
                                                                    E.text "disabled"
                                                 }
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
                                      selectStyle
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
                       , E.el [] <|
                            settingWidthHeight model
                       , E.el [centerX] <|
                          if ((Maybe.withDefault 0 (String.toInt model.campusSetting.tempWidth )) > 2) && ((Maybe.withDefault 0 (String.toInt model.campusSetting.tempHeight)) > 2) then
                              Input.button [ htmlAttribute <| HAttrs.style "color" "white"
                                           ]
                                           { onPress = Just (ChangePixelSize model.campusSetting.tempWidth model.campusSetting.tempHeight)
                                           , label = E.el [ Font.color <| shiroIro
                                                          , Font.size <| 14
                                                          ] <|
                                                              E.text "Apply"
                                           }
                          else
                              Input.button [ htmlAttribute <| HAttrs.style "opacity" "0.6" 
                                           , htmlAttribute <| HAttrs.style "color" "white"
                                           ]
                                           { onPress = Just ForDisabled
                                           , label = E.el [ Font.color <| shiroIro
                                                          , Font.size <| 14
                                                          ] <| 
                                                              E.text "disabled"
                                           }
                       ]
              -- Positon --           
              , Input.button [ alignBottom 
                             , Font.color <| shiroIro
                             ]
                             { onPress = Just <| ChangePosition SettingPanel
                             , label = E.text <| changePositionText model.settingPosition
                             }
              ]
              
    else
        E.none

selectStyle =
    let
        handler selectedValue =
            Change selectedValue
        tempOption : String -> String -> Html Msg
        tempOption value_ text_ =
            option [ value value_ ] [ H.text text_ ]
    in
        div [ HAttrs.style "height" "14px", HAttrs.style "font-size" "14px" ]
            [ H.select [ onChange handler ]
                       [ tempOption "solid 1px" "solid"
                       , tempOption "none" "none"
                       , tempOption "double" "double"
                       , tempOption "groove" "groove"
                       , tempOption "ridge" "ridge"
                       , tempOption "dashed 1px" "dashed"
                       , tempOption "dotted 1px" "dotted"
                       ]
            ]

settingWidthHeight : Model -> Element Msg
settingWidthHeight model =
    E.el [] <|
        html <|
            div []
                [ div [ HAttrs.style "color" "white" 
                      , HAttrs.style "font-size" "14px"
                      ]
                      [ H.text "width : "
                      , H.input [ HAttrs.style "" ""
                                , HAttrs.style "width" "30px"
                                , HAttrs.style "height" "14px"
                                , onInput SetPixelWidth
                                ] 
                                []
                      ] 
                , div [ HAttrs.style "color" "white" 
                      , HAttrs.style "font-size" "14px"
                      ]
                      [ H.text "height : "
                      , H.input [ HAttrs.style "width" "30px" 
                                , HAttrs.style "height" "14px"
                                , onInput SetPixelHeight
                                ]
                                []
                      ] 
                ]

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
                                                       , if not <| (isCorrectWidthHeight model) then BBtn.primary else BBtn.secondary
                                                       , BBtn.attrs [ onClick CreateCampus ]
                                                       , BBtn.disabled <| isCorrectWidthHeight model
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
    model.palette
        |> Array.fromList
        |> Array.get n
        |> Maybe.withDefault ""


getCampusColor : Model -> Int -> Int -> String
getCampusColor model x y =
    model.campus
        |> Array.fromList
        |> Array.get x
        |> Maybe.withDefault [(0, "")]
        |> Array.fromList
        |> Array.get y
        |> Maybe.withDefault (0, "")
        |> Tuple.second


getCampusInt : Model -> Int -> Int
getCampusInt model n =
    model.campus
        |> Array.fromList
        |> Array.get n
        |> Maybe.withDefault [(0, "")]
        |> Array.fromList
        |> Array.get n
        |> Maybe.withDefault (0, "")
        |> Tuple.first

makeTable : Model -> Int -> Int -> Html Msg
makeTable model width height =
    div []
        [ H.table [ HAttrs.style "table-layout" "fixed", HAttrs.style "border-collapse" "collapse"] <| 
            List.map( \y -> tr [] <| 
                List.map( \x -> td [ HAttrs.style "width" (model.campusSetting.width ++ "px")
                                   , HAttrs.style "height" (model.campusSetting.height ++ "px")
                                   , HAttrs.style "border" (model.campusSetting.borderColor ++ " " ++ model.campusSetting.borderStyle)--model.campusSetting.border
                                   , HEvents.onClick (ChangeColor y x model.mainPalette)
                                   , HAttrs.style "background-color" (getCampusColor model y x)
                                   ] 
                                   [] 
                        ) <| 
                            List.range 0 (width-1)
                    ) <| 
                        List.range 0 (height-1) 
        ]

updateCampus : Model -> Int -> Int -> String -> List(List (Int, String))
updateCampus model x y color =
    List.append
        (List.append 
                (List.take x model.campus) 
                (List.singleton <|
                    List.append
                        ((++)
                            (List.take y <| 
                                Maybe.withDefault [(0, "")] <|
                                    Array.get x <| 
                                        Array.fromList model.campus
                            )
                            (List.singleton (getCampusInt model <| y
                                            , color
                                            )
                            )
                        )
                        (List.drop (y+1) <| 
                                Maybe.withDefault [(0, "")] <| 
                                    Array.get x <| 
                                        Array.fromList model.campus
                        )
                ) 
        )
        (List.drop (x+1) model.campus)


addColorToPalette : Model -> String -> List String
addColorToPalette model color =
    List.append [color] model.palette

--setBorderColor : Model -> String -> 

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
                     List.range 1 (List.length model.palette)

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

        
isCorrectWidthHeight : Model -> Bool
isCorrectWidthHeight model =
    let
        chkInt : Bool
        chkInt =
            Maybe.withDefault 0 (String.toInt model.tempCampusSize.width)
            * 
            Maybe.withDefault 0 (String.toInt model.tempCampusSize.height) 
            > 0

        chkLength : Bool
        chkLength =
            Maybe.withDefault 0 (String.toInt model.tempCampusSize.width) <= 64 
            && 
            Maybe.withDefault 0 (String.toInt model.tempCampusSize.height) <= 64
    in
        not <| 
            chkInt && chkLength

createCampusButton model =
    if (isCorrectWidthHeight model) then
        Input.button [] 
                     { onPress = Just CreateCampus
                     , label = (E.text "Create!")
                     }
    else
        Input.button [ Region.description "fuck you"
                     , Background.color (rgb255 84 84 84)
                     ]
                     { onPress = Just ForDisabled
                     , label = (E.text "Create!")
                     }


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

--ColorSet--
rouIro = rgb255 43 43 43
sumiIro = rgb255 89 88 87
shiroIro = rgb255 255 255 255
shironeriIro = rgb255 243 243 242
shironezuIro = rgb255 220 221 221
