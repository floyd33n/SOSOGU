module View exposing (..)

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
import Dict exposing (..)
import Element as E exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Font as Font exposing (..)
import Element.Input as Input exposing (..)
import Element.Region as Region exposing (..)
import Html as H exposing (..)
import Html.Attributes as HAttrs exposing (..)
import Html.Events as HEvents exposing (..)
import Html.Lazy as HLazy exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import Savedata exposing (..)
import Svg exposing (..)
import Svg.Attributes as SAttrs exposing (..)
import Svg.Events as SEvents exposing (..)
import Types exposing (..)
import Utilities exposing (..)



{-
   viewSubPalette : SubPalette -> Html Msg
   viewSubPalette subPalette =
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
-}
-- Display button to select campus position


viewSelectCampusPositionButton : Setting -> Html Msg
viewSelectCampusPositionButton tempSetting =
    let
        tempDiv : CampusPosition -> Html Msg
        tempDiv position_ =
            div
                [ HEvents.onClick (InputCampusPosition position_)
                , HAttrs.style "border" "none"
                , HAttrs.style "width" "15px"
                , HAttrs.style "height" "15px"
                , if tempSetting.panelPosition.campusPanel == position_ then
                    HAttrs.style "background-color" "#f0ad00"

                  else
                    HAttrs.style "background-color" "white"
                , HAttrs.style "float" "left"
                , HAttrs.style "border" "solid 1px #5a6378"
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



-- Display buttons to select setting panel & palette panel position


viewSelectSettingAndPalettePositionButtons : Panel -> Setting -> Html Msg
viewSelectSettingAndPalettePositionButtons panel_ tempSetting =
    let
        temp_ : Position -> Html Msg
        temp_ position_ =
            div
                [ HEvents.onClick (InputSettingAndPalettePosition panel_ position_)
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
                , HAttrs.style "border" "solid 1px #5a6378"
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
    div []
        [ temp_ Left
        , temp_ Right
        ]



-- Display horizontal separator for the panel


horizontalSeparatorForPanels : Element Msg
horizontalSeparatorForPanels =
    E.el
        [ centerX
        , E.width <| px 80
        , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
        , Border.color <| shiroIro
        ]
    <|
        E.none



-- Display campus


viewCampus : Model -> Html Msg
viewCampus model =
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
                    [ SAttrs.stroke "black" ]
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
                                        , HAttrs.style "width" ("calc(1px + " ++ model.setting.pixelSize.width ++ "px)")
                                        , HAttrs.style "height" ("calc(1px + " ++ model.setting.pixelSize.height ++ "px)")
                                        , HAttrs.style "margin" "-1px"
                                        , HAttrs.style "display" "flex"
                                        ]
                                    <|
                                        [ Svg.svg
                                            [ SAttrs.viewBox ("0, 0, " ++ model.setting.pixelSize.width ++ ", " ++ model.setting.pixelSize.height)
                                            , SAttrs.width (model.setting.pixelSize.width ++ "px")
                                            , SAttrs.height (model.setting.pixelSize.height ++ "px")
                                            ]
                                            [ rect
                                                (List.append
                                                    [ SAttrs.strokeWidth "1"
                                                    , SAttrs.fill <| getCampusColor model.campus ( x, y )
                                                    , SAttrs.width (model.setting.pixelSize.width ++ "px")
                                                    , SAttrs.height (model.setting.pixelSize.height ++ "px")
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
                                List.range 0 (model.campusSize.width - 1)
                    )
                <|
                    List.range 0 (model.campusSize.height - 1)
            ]

    else
        div [] []


viewToolsPanel : Model -> Element Msg
viewToolsPanel model =
    let
        buttonAttrs =
            [ Border.color shiroIro
            , Border.widthEach { top = 0, right = 0, left = 0, bottom = 1 }
            , E.height <| px 18
            , Font.color shiroIro
            , Font.size 14
            , centerY
            ]

        viewUndoButton : Element Msg
        viewUndoButton =
            Input.button
                buttonAttrs
                { onPress =
                    let
                        x =
                            model.history
                                |> Dict.get (Dict.size model.history - 1)
                                |> Maybe.withDefault ( ( 0, 0 ), "white" )
                                |> Tuple.first
                                |> Tuple.first

                        y =
                            model.history
                                |> Dict.get (Dict.size model.history - 1)
                                |> Maybe.withDefault ( ( 0, 0 ), "white" )
                                |> Tuple.first
                                |> Tuple.second
                    in
                    Just <| Undo ( y, x )
                , label = E.text "Undo"
                }

        saveButton : Element Msg
        saveButton =
            Input.button
                buttonAttrs
                { onPress = Just ShowSaveModalWindow
                , label = E.text "Save"
                }

        newButton : Element Msg
        newButton =
            Input.button
                buttonAttrs
                { onPress = Just NewProject
                , label = E.text "New"
                }

        verticalSeparatorForToolsPanel : Element Msg
        verticalSeparatorForToolsPanel =
            E.el
                [ E.width <| px 1
                , E.height <| px 16
                , Border.color <| shiroIro
                , Border.widthEach { top = 0, right = 1, left = 0, bottom = 0 }
                ]
            <|
                E.none
    in
    E.row
        [ E.width E.fill
        , E.height <| px 36
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
            , verticalSeparatorForToolsPanel
            , saveButton
            , verticalSeparatorForToolsPanel
            , viewUndoButton
            ]
        ]



-- Display closed setting panel


viewClosedSettingPanel : Element Msg
viewClosedSettingPanel =
    column
        [ E.width <| px 50
        , E.height E.fill
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



-- Dispaly opened setting panel


viewOpenedSettingPanel : Model -> Element Msg
viewOpenedSettingPanel model =
    column
        [ E.width <| px 110
        , E.height E.fill
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
        , horizontalSeparatorForPanels

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
            , horizontalSeparatorForPanels
            , E.el
                [ centerX
                ]
              <|
                html <|
                    BInput.text
                        [ BInput.onInput InputBorderColor
                        , if isColor model.temp.setting.borderColor then
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
                            if isColor model.temp.setting.borderColor then
                                ""

                            else if String.isEmpty model.temp.setting.borderColor then
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
            , horizontalSeparatorForPanels
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
                                , HAttrs.selected <| value_ == model.setting.borderStyle
                                ]
                                [ H.text text_ ]
                    in
                    BSelect.select
                        [ BSelect.onChange SelectBorderStyle
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
            , horizontalSeparatorForPanels
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
                            [ BInput.onInput InputPixelWidth
                            , BInput.small
                            , if Maybe.withDefault 0 (String.toInt model.temp.setting.pixelSize.width) > 0 then
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
                        [ H.text <| pixelSizeErr model.temp.setting.pixelSize.width ]
                , E.row
                    [ Font.color <| shiroIro
                    , Font.size <| 14
                    , centerX
                    ]
                    [ E.el [] <|
                        E.text "height : "
                    , html <|
                        BInput.text
                            [ BInput.onInput InputPixelHeight
                            , BInput.small
                            , if Maybe.withDefault 0 (String.toInt model.temp.setting.pixelSize.height) > 0 then
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
                        [ H.text <| pixelSizeErr model.temp.setting.pixelSize.height ]
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
            , horizontalSeparatorForPanels
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
                            viewSelectSettingAndPalettePositionButtons SettingPanel model.temp.setting
                    ]
                , column
                    [ centerX
                    , E.spacing 4
                    ]
                    [ E.el [ centerX ] <|
                        E.text "Palette"
                    , E.el [ centerX ] <|
                        html <|
                            viewSelectSettingAndPalettePositionButtons PalettePanel model.temp.setting
                    ]
                , column
                    [ centerX
                    , E.spacing 4
                    ]
                    [ E.el [ centerX ] <|
                        E.text "Campus"
                    , E.el [ centerX ] <|
                        html <|
                            viewSelectCampusPositionButton model.temp.setting
                    ]
                ]
            ]
        , E.el
            [ centerX
            , paddingEach { top = 20, right = 0, left = 0, bottom = 0 }
            ]
          <|
            if isCorrectSetting model.temp.setting then
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


viewPalettePanel : Model -> Element Msg
viewPalettePanel model =
    column
        [ E.width <| px 110
        , E.height E.fill
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
        , horizontalSeparatorForPanels
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
            , horizontalSeparatorForPanels
            , E.el
                [ centerX
                ]
              <|
                html <|
                    BInput.text
                        [ BInput.onInput InputColorValue
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
            , horizontalSeparatorForPanels
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
            , horizontalSeparatorForPanels
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



{- Campus is displayed in this Panel -}


viewCampusPanel : Model -> Element Msg
viewCampusPanel model =
    let
        campusPositionAttrs : List (E.Attribute Msg)
        campusPositionAttrs =
            case model.setting.panelPosition.campusPanel of
                TopCenter ->
                    [ centerX
                    , alignTop
                    , padding 3
                    ]

                TopRight ->
                    [ E.alignRight
                    , alignTop
                    , padding 3
                    ]

                TopLeft ->
                    [ E.alignLeft
                    , alignTop
                    , padding 3
                    ]
    in
    column
        [ E.width E.fill
        , E.height E.fill
        , Background.color <| shironezuIro
        ]
        [ viewToolsPanel model
        , E.el campusPositionAttrs <|
            html <|
                viewCampus model
        ]


viewSettingPanel : Model -> Element Msg
viewSettingPanel model =
    case model.settingPanelStatus of
        Open ->
            viewOpenedSettingPanel model

        Close ->
            viewClosedSettingPanel


viewPanels : Model -> Element Msg
viewPanels model =
    let
        verticalSeparator =
            E.el
                [ E.width <| px 1
                , E.height E.fill
                , Border.widthEach { top = 0, right = 1, left = 0, bottom = 0 }
                , Border.color shiroIro
                ]
            <|
                E.none

        viewPanels_ : List (Element Msg)
        viewPanels_ =
            case
                ( model.setting.panelPosition.settingPanel
                , model.setting.panelPosition.palettePanel
                )
            of
                ( Left, Right ) ->
                    [ viewSettingPanel model
                    , verticalSeparator
                    , viewCampusPanel model
                    , verticalSeparator
                    , viewPalettePanel model
                    ]

                ( Right, Left ) ->
                    [ viewPalettePanel model
                    , verticalSeparator
                    , viewCampusPanel model
                    , verticalSeparator
                    , viewSettingPanel model
                    ]

                ( Right, Right ) ->
                    [ viewCampusPanel model
                    , verticalSeparator
                    , viewPalettePanel model
                    , verticalSeparator
                    , viewSettingPanel model
                    ]

                ( Left, Left ) ->
                    [ viewSettingPanel model
                    , verticalSeparator
                    , viewPalettePanel model
                    , verticalSeparator
                    , viewCampusPanel model
                    ]
    in
    E.row
        [ E.width E.fill
        , E.height E.fill
        ]
        viewPanels_



-- Modal Windows--


viewSaveAndNewCampusModalWindow : Model -> Html Msg
viewSaveAndNewCampusModalWindow model =
    BGrid.container []
        [ BModal.config CloseSaveModalWindow
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
                                    , BBtn.onClick (ShowSaveEditingCampusModalWindow No)
                                    ]
                                    [ H.text "New Campus" ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> BModal.view model.saveAndNewCampusModalWindow
        ]


viewSaveModalWindow : Model -> Html Msg
viewSaveModalWindow model =
    BGrid.container []
        [ BModal.config CloseSaveModalWindow
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


createCampusModalWindow : Model -> Html Msg
createCampusModalWindow model =
    let
        viewLoadSavedataErr : Html Msg
        viewLoadSavedataErr =
            case model.loadedSavedata of
                "" ->
                    H.text ""

                _ ->
                    if isSavedata model.loadedSavedata then
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
        [ BModal.config CloseCreateCampusModalWindow
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
                                [ BInput.onInput InputCampusWidth
                                , BInput.attrs
                                    [ HAttrs.style "width" "90px"
                                    , HAttrs.style "height" "30px"
                                    ]
                                ]
                            ]
                        , BGrid.col [ BCol.xs5 ]
                            [ div [ HAttrs.style "margin" "0 auto" ]
                                [ H.text "Height" ]
                            , BInput.number
                                [ BInput.onInput InputCampusHeight
                                , BInput.attrs
                                    [ HAttrs.style "width" "90px"
                                    , HAttrs.style "height" "30px"
                                    ]
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
                                , BBtn.disabled <| not (isCorrectWidthHeight model.temp.campusSize.width model.temp.campusSize.height)
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


viewConfirmSaveCampusModalWindow : Model -> Html Msg
viewConfirmSaveCampusModalWindow model =
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
                                    , BBtn.onClick (ShowSaveEditingCampusModalWindow No)
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
                                    , BBtn.onClick (ShowSaveEditingCampusModalWindow Yes)
                                    ]
                                    [ H.text "Yes" ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> BModal.view model.saveEditingCampusModalWindow
        ]
