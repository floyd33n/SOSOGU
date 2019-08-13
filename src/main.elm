module Main exposing(main)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Array exposing (..)
import Debug exposing (..)
import Regex exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }

--MODEL--
type alias Model = 
  { campus : List (List (Int, String))
  , colorValue : String
  , palette : List String
  , mainPalette : String
  , campusSize : CampusSize
  , tempCampusSize : CampusSize
  }

type alias CampusSize =
    { width : Int
    , height : Int
     }

--INIT--
init : Model
init =
    Model [[(0, "")]] "" [] "" (CampusSize -1 -1) (CampusSize 0 0)
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
            { model | tempCampusSize = { width = Maybe.withDefault 1 (String.toInt width), height = model.tempCampusSize.height } }
        
        SetCampusHeight height ->
            { model | tempCampusSize = { height = Maybe.withDefault 1 (String.toInt height), width = model.tempCampusSize.width } }
       
        CreateCampus ->
             { model | campusSize = model.tempCampusSize
                     , campus = List.repeat model.tempCampusSize.height (Array.toIndexedList (Array.fromList (List.repeat model.tempCampusSize.width "white")))
             }
--VIEW--
view : Model -> Html Msg
view model =
    div []
        [ div [ (id "campus"), (style "float" "left") ]
            [ div []
                [ input [ (type_ "number"), (style "width" "45px"), (placeholder "width"), (onInput (SetCampusWidth)) ] []
                , input [ (type_ "number"), (style "width" "45px"), (placeholder "height"), (onInput (SetCampusHeight)) ] []
                , button [ (onClick CreateCampus), (disabled (chkWidthHeightField model)) ] [ text "Create Campus" ]
                ]
            , makeTable model model.campusSize.width model.campusSize.height
            ]
        , div [ (id "palette"), (style "float" "right") ]
            [ div []
                [ input [ (placeholder "Color"), (onInput ColorValue ) ] []
                , button [(onClick (AddColorToPalette model.colorValue)), (disabled (chkColorField model)) ] [ text "Add" ]
                , div [ (id "main_palette"), (style "background-color" model.mainPalette) ] []
                ]
            , displayPalette model
            ]
        , input [] []
        ]

--FUNC--
{-
campusSizeField : Model -> Html Msg
campusSizeField model =
    div []
        [ input [ (placeholder "Width"), (onInput SetCampusWidth) ] []
        --, input [ (placeholder "Height"), (onInput model.campusSize.height) ]
        ]
  -}      
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
        [ table []
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
            [ div [ (id "palette_square"), (onClick (SetMainPalette (plt - 1))), (style "background-color" (getPaletteColor model (plt - 1))) ] [ text (String.fromInt plt) ]
            , div [ id "palette_color_name" ] [text (getPaletteColor model (plt - 1)) ]
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
    not ((model.tempCampusSize.width * model.tempCampusSize.height) > 0) || not (( model.tempCampusSize.width <= 64 ) && ( model.tempCampusSize.height <= 64 ))
