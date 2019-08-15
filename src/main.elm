module Main exposing(main)
import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Array exposing (..)
import Debug exposing (..)
import Regex exposing (..)
-- elm-ui --
import Element as Ele exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Region as Region

main =
  Browser.sandbox { init = init, update = update, view = view }


--MODEL--
type alias Model = 
  { campus : List (List (Int, String))
  , colorValue : String
  , palette : List String
  , mainPalette : String
  , campusSize : CampusSize
  , tempCampusSize : TempCampusSize
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
init : Model
init =
    Model [[(0, "")]] "" [] "White" (CampusSize 0 0) (TempCampusSize "" "")
{-
initCampus : List (List (Int, String))
initCampus =
    List.repeat (8) (Array.toIndexedList (Array.fromList (List.repeat (8) "white")))
-}
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

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeColor x y color ->
            { model | campus = updateCampus model x y color}

        ColorValue value ->
            { model | colorValue = (String.toLower value) }

        AddColorToPalette color ->
            { model | palette = addColorToPalette model color }
           
        SetMainPalette n ->
            { model | mainPalette = getPaletteColor model n }
        
        SetCampusWidth width ->
            { model | tempCampusSize = { width = width, height = model.tempCampusSize.height } }
        
        SetCampusHeight height ->
            { model | tempCampusSize = { height = height, width = model.tempCampusSize.width } }
       
        CreateCampus ->
             { model | campusSize = { width = (Maybe.withDefault 0 (String.toInt model.tempCampusSize.width)), height = (Maybe.withDefault 0 (String.toInt model.tempCampusSize.height)) }
                     , campus = List.repeat model.campusSize.height (Array.toIndexedList (Array.fromList (List.repeat model.campusSize.width "white")))
             }
          
        DisabledCreateCampus -> 
            { model | colorValue = model.colorValue }

--VIEW--
view : Model -> Html Msg
view model =
  div [ style "height" "100%"  ] [ layout [explain Debug.todo] <|
            column [ Ele.width fill, Ele.height fill, explain Debug.todo ]
                [ row [ Ele.width fill, Ele.height <| px 100 ] [ Ele.text "header" ]
                , row [ Ele.width fill, Ele.height fill, explain Debug.todo ]
                    [ column [ Ele.width <| px 100, Ele.height fill ] [ Ele.text "setting"
                                                                      , Input.text [] { onChange = SetCampusWidth
                                                                                      , text = model.tempCampusSize.width
                                                                                      , placeholder = Just (Input.placeholder [] (Ele.text "Width"))
                                                                                      , label = (Input.labelHidden "?")
                                                                                      }
                                                                      , Input.text [] { onChange = SetCampusHeight
                                                                                      , text = model.tempCampusSize.height
                                                                                      , placeholder = Just (Input.placeholder [] (Ele.text "Height"))
                                                                                      , label = (Input.labelHidden "?")
                                                                                      }
                                                                      , createCampusButton model
                                                                      ]
                    , column [ Ele.width <| px 100, Ele.height fill ] [ Ele.text "palette"
                                                                      , Input.text [] { onChange = ColorValue
                                                                                      , text = model.colorValue
                                                                                      , placeholder = Just (Input.placeholder [] (Ele.text "Color"))
                                                                                      , label = (Input.labelHidden "?")
                                                                                      }
                                                                      , addColorButton model
                                                                      , html (div [ (id "main_palette"), (style "background-color" model.mainPalette) ] [] )
                                                                      , html (displayPalette model)
                                                                      ]
                    , column [ Ele.width fill, Ele.height fill ] [ Ele.text "campus"
                                                                 , html ( makeTable model model.campusSize.width model.campusSize.height )
                                                                 ]
                    ]
                , row [ Ele.width fill, Ele.height <| px 100 ] [ Ele.text "footer" ]
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
{-
testTable : Int -> Int -> Html Msg
testTable width height =
    div []
        [ Html.table []
            <| List.map(\y -> tr[]
                <| List.map(\x -> td [])
                    <| List.range 0 (width-1))
                        <| List.range 0 (height-1) ]
-}

makeTable : Model -> Int -> Int -> Html Msg
makeTable model width height =
    div []
        [ Html.table []
            <| List.map(\y -> tr[]
                <| List.map(\x -> td [onClick(ChangeColor y x model.mainPalette), style "background-color" (getCampusColor model y x)] [ {-text ((String.fromInt y) ++ "," ++ (String.fromInt x)) -}])
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
            [ div [ (id "palette_square"), (onClick (SetMainPalette (plt - 1))), (style "background-color" (getPaletteColor model (plt - 1))) ] [ Html.text (String.fromInt plt) ]
            , div [ id "palette_color_name" ] [ Html.text (getPaletteColor model (plt - 1)) ]
            ])
                <| List.range 1 (List.length model.palette)

chkColorField : Model -> Bool
chkColorField model =
    let
        chkHexColorCode : String -> Bool
        chkHexColorCode hex =
            Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "[g-z]") hex
        
        chkColorCodeLength : Bool
        chkColorCodeLength =
            (not ((String.length model.colorValue) == 4)) && (not ((String.length model.colorValue) == 7))

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

        chkColorName : Bool
        chkColorName =
            List.member model.colorValue cssColorNames
    in
        (String.isEmpty model.colorValue) || (not chkColorName) && chkColorCodeLength

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
                        , label = (Ele.text "Create!")
                        }
    else
        Input.button [ Region.description "fuck you"
                     , Background.color (rgb255 84 84 84)
                     ]
                     { onPress = Just DisabledCreateCampus
                     , label = (Ele.text "Create!")
                     }

addColorButton model =
    if (not (chkColorField model)) then
        Input.button [] { onPress = Just (AddColorToPalette model.colorValue)
                        , label = (Ele.text "Add")
                        }
    else
        Input.button [ Region.description "fuck"
                     , Background.color (rgb255 84 84 84)
                     ]
                     { onPress = Just DisabledCreateCampus
                     , label = (Ele.text ";_;")
                     }
