module Utilities exposing (..)

import Dict exposing (..)
import Html as H exposing (node)
import Html.Attributes as HA exposing (href, rel)
import Types exposing (..)


loadCssElmReactor path =
    H.node "link" [ HA.rel "stylesheet", HA.href path ] []


getCampusColor : Campus -> Point -> String
getCampusColor campus ( x, y ) =
    Dict.get ( x, y ) campus
        |> Maybe.withDefault "white"


getSubPaletteColor : SubPalette -> Serial -> CssColor
getSubPaletteColor subPalette n =
    Dict.get n subPalette
        |> Maybe.withDefault "white"
