module Utilities exposing (..)

import Dict exposing (..)
import Element as E exposing (..)
import Html as H exposing (node)
import Html.Attributes as HA exposing (href, rel)
import Regex exposing (..)
import Types exposing (..)


loadCssElmReactor path =
    H.node "link" [ HA.rel "stylesheet", HA.href path ] []


isAllowProjectName : String -> Bool
isAllowProjectName name =
    let
        incSlash =
            String.contains "/" name

        incColon =
            String.contains ":" name

        incAsterisk =
            String.contains "*" name

        incQuestion =
            String.contains "?" name

        incLessThan =
            String.contains "<" name

        incGreaterThan =
            String.contains ">" name

        incPipe =
            String.contains "|" name
    in
    not <|
        List.member True
            [ incSlash, incColon, incAsterisk, incQuestion, incLessThan, incGreaterThan, incPipe ]


isCorrectPixelSize : PixelSize -> Bool
isCorrectPixelSize p =
    let
        sizeList =
            List.map (\n -> Maybe.withDefault 0 (String.toInt n)) [ p.width, p.height ]

        isFrom5To100 n =
            5 <= n && 100 >= n
    in
    List.map isFrom5To100 sizeList == [ True, True ]


isCorrectSetting : TempSetting -> Bool
isCorrectSetting tempSetting =
    isColor tempSetting.borderColor && isCorrectPixelSize tempSetting.pixelSize


isCorrectCampusSize : TempCampusSize -> Bool
isCorrectCampusSize c =
    let
        sizeList =
            List.map (\n -> Maybe.withDefault 0 (String.toInt n)) [ c.width, c.height ]

        isFrom1To100 n =
            1 <= n && 100 >= n
    in
    List.map isFrom1To100 sizeList == [ True, True ]


isWarningCampusSize : TempCampusSize -> Bool
isWarningCampusSize c =
    let
        sizeList =
            List.map (\n -> Maybe.withDefault 0 (String.toInt n)) [ c.width, c.height ]

        isGTE50 n =
            50 <= n
    in
    List.map isGTE50 sizeList == [ True, True ]


getCampusColor : Campus -> Point -> String
getCampusColor campus ( x, y ) =
    Dict.get ( x, y ) campus
        |> Maybe.withDefault "white"


getSubPaletteColor : SubPalette -> Serial -> CssColor
getSubPaletteColor subPalette n =
    Dict.get n subPalette
        |> Maybe.withDefault "white"


getHistoryColor : History -> Point -> CssColor
getHistoryColor history ( x, y ) =
    history
        |> Dict.get (Dict.size history)
        |> Maybe.withDefault ( ( 0, 0 ), "white" )
        |> Tuple.second


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
