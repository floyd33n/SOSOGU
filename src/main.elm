module Main exposing(main)
import Browser
import Html as H exposing (..)
import Html.Attributes as HAttrs exposing(..)
import Html.Events as HEvents exposing (..)
import Array exposing (..)
import Debug exposing (..)
import Regex exposing (..)
import Result.Extra as ExResult exposing  (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Region as Region
import Bootstrap.Button as BBtn exposing (..)
css path =
  node "link" [rel "stylesheet", href path] []

--MODEL--
type alias Model = 
  { campus : List (List (Int, String))
  , colorValue : String
  , palette : List String
  , mainPalette : String
  , campusSize : CampusSize
  , tempCampusSize : TempCampusSize
  , modalVisibility : Modal.Visibility
  }

type alias TempCampusSize =
    { width : String
    , height : String
    }

type alias CampusSize =
    { width : Int
    , height : Int
     }

--INIT--
init : () -> (Model, Cmd Msg)
init _ =
    ( Model
      [[(0, "")]] --campus
      "" --colorValue
      [] --palette
      "White" --mainpalette
      (CampusSize 0 0) --campusSize
      (TempCampusSize "" "") --tempCampusSize
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
    | DisabledCreateCampus

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
                  ,         campusSize = { width = (Maybe.withDefault 0 (String.toInt model.tempCampusSize.width)), height = (Maybe.withDefault 0 (String.toInt model.tempCampusSize.height)) }
                  }
                , Cmd.none
                )

        DisabledCreateCampus -> 
            ( { model | colorValue = model.colorValue }
            , Cmd.none
            )

--VIEW--
view : Model -> Html Msg
view model =
    div [ style "height" "100%"] [ css <| "../style.css", layout [explain Debug.todo] <|
            column [ E.width fill, E.height fill, explain Debug.todo ]
                [ row [ explain Debug.todo, E.width fill, E.height <| px 100 ] [ E.el [alignLeft] <| E.text "SOSOGU"
                                                                               , E.el [alignRight, E.width <| px 100 ] <| E.text "nav"
                                                                               ]
                , row [ E.width fill, E.height fill, explain Debug.todo ]
                    [ column [ E.width <| px 100, E.height fill, htmlAttribute <| id "setting-bak"] [ E.text "setting"
                                                                      , Input.text [] { onChange = SetCampusWidth
                                                                                      , text = model.tempCampusSize.width
                                                                                      , placeholder = Just (Input.placeholder [] (E.text "Width"))
                                                                                      , label = (Input.labelHidden "")
                                                                                      }
                                                                      , Input.text [] { onChange = SetCampusHeight
                                                                                      , text = model.tempCampusSize.height
                                                                                      , placeholder = Just (Input.placeholder [] (E.text "Height"))
                                                                                      , label = (Input.labelHidden "")
                                                                                      }
                                                                      , createCampusButton model
                                                                      ]
                    , column [E.width <| px 100, E.height fill, htmlAttribute <| id "palette-bak"] [ E.text "palette"
                                                                      , Input.text [] { onChange = ColorValue
                                                                                      , text = model.colorValue
                                                                                      , placeholder = Just (Input.placeholder [] (E.text "Color"))
                                                                                      , label = (Input.labelHidden "?")
                                                                                      }
                                                                      , addColorButton model
                                                                      , html (div [ (id "main_palette"), (style "background-color" model.mainPalette) ] [] )
                                                                      , html (displayPalette model)
                                                                      ]
                    , column [ E.width fill, E.height fill, htmlAttribute <| id "campus-bak"] [ E.text "campus"
                                                                 , html ( makeTable model model.campusSize.width model.campusSize.height )
                                                                 ]
                    ]
                , row [ explain Debug.todo, E.width fill, E.height <| px 10 ] [E.text "footer" ]
                ]
         ]
--FUNC--
getPaletteColor : Model -> Int -> String
getPaletteColor model n =
    model.palette
        |> Array.fromList
        |> Array.get n
        |> Maybe.withDefault "unknown"


getCampusColor : Model -> Int -> Int -> String
getCampusColor model x y =
    model.campus
        |> Array.fromList
        |> Array.get x
        |> Maybe.withDefault [(-2, "_")]
        |> Array.fromList
        |> Array.get y
        |> Maybe.withDefault (-2, "_")
        |> Tuple.second


getCampusInt : Model -> Int -> Int
getCampusInt model n =
    model.campus
        |> Array.fromList
        |> Array.get n
        |> Maybe.withDefault [(-2, "_")]
        |> Array.fromList
        |> Array.get n
        |> Maybe.withDefault (-2, "_")
        |> Tuple.first

makeTable : Model -> Int -> Int -> Html Msg
makeTable model width height =
    div []
        [ H.table []
            <| List.map(\y -> tr[]
                <| List.map(\x -> td [ HEvents.onClick (ChangeColor y x model.mainPalette), style "background-color" (getCampusColor model y x)] [] )
                    <| List.range 0 (width-1))
                        <| List.range 0 (height-1) ]

updateCampus : Model -> Int -> Int -> String -> List(List (Int, String))
updateCampus model x y color =
    List.append
        (List.append (List.take x model.campus) 
                     ( (List.singleton (List.append
                        (List.append (List.take y (Maybe.withDefault [(0, "")] (Array.get x (Array.fromList model.campus))))
                                     (List.singleton ((getCampusInt model y), color))
                                                                    )
                                                       (List.drop (y+1) (Maybe.withDefault [(999, "_____")] (Array.get x (Array.fromList model.campus))))
                                 )) )
                    )
                    (List.drop (x+1) model.campus)

addColorToPalette : Model -> String -> List String
addColorToPalette model color =
    List.append [color] model.palette

displayPalette : Model -> Html Msg
displayPalette model =
    div []
        <| List.map (\plt -> div []
            [ div [ (id "palette_square"), ( HEvents.onClick (SetMainPalette (plt - 1))), (style "background-color" (getPaletteColor model (plt - 1))) ] [ H.text (String.fromInt plt) ]
            , div [ id "palette_color_name" ] [ H.text (getPaletteColor model (plt - 1)) ]
            ])
                <| List.range 1 (List.length model.palette)

isColor : Model -> Bool
isColor model =
    case (String.left 1 model.colorValue) of
        "#" ->
            not (Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "[g-z]" ) (String.dropLeft 1 model.colorValue) )
              && (String.length model.colorValue == 4)
                || (String.length model.colorValue == 7)

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
                    List.member model.colorValue cssColorNames
            in
                isColorName

        

chkWidthHeightField : Model -> Bool
chkWidthHeightField model =
    let
        chkInt : Bool
        chkInt =
            (Maybe.withDefault 0 (String.toInt model.tempCampusSize.width)) * (Maybe.withDefault 0 (String.toInt model.tempCampusSize.height)) > 0

        chkLength : Bool
        chkLength =
            (Maybe.withDefault 0 (String.toInt model.tempCampusSize.width)) <= 64 && (Maybe.withDefault 0 (String.toInt model.tempCampusSize.height)) <= 64
    in
        chkInt && chkLength

createCampusButton model =
    if (chkWidthHeightField model) then
        Input.button [] { onPress = Just CreateCampus
                        , label = (E.text "Create!")
                        }
    else
        Input.button [ Region.description "fuck you"
                     , Background.color (rgb255 84 84 84)
                     ]
                     { onPress = Just DisabledCreateCampus
                     , label = (E.text "Create!")
                     }

addColorButton model =
    if ((isColor model)) then
        Input.button [] { onPress = Just (AddColorToPalette model.colorValue)
                        , label = (E.text "Add")
                        }
    else
        Input.button [ Region.description "fuck"
                     , Background.color (rgb255 84 84 84)
                     ]
                     { onPress = Just DisabledCreateCampus
                     , label = (E.text ";_;")
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
