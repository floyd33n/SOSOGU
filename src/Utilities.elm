module Utilities exposing (..)

import Dict exposing (..)
import Element as E exposing (..)
import Html as H exposing (node)
import Html.Attributes as HA exposing (href, rel)
import Regex exposing (..)
import Types exposing (..)


loadCssElmReactor path =
    H.node "link" [ HA.rel "stylesheet", HA.href path ] []


isCorrectSetting : Setting -> Bool
isCorrectSetting setting =
    isColor setting.borderColor && isCorrectWidthHeight setting.width setting.height


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


getCampusColor : Campus -> Point -> String
getCampusColor campus ( x, y ) =
    Dict.get ( x, y ) campus
        |> Maybe.withDefault "white"


getSubPaletteColor : SubPalette -> Serial -> CssColor
getSubPaletteColor subPalette n =
    Dict.get n subPalette
        |> Maybe.withDefault "white"


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
