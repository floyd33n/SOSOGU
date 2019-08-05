module Main exposing(main)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Array exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }

--MODEL--
type alias Model = 
  { campus : Campus
  , colorValue : String
  }

init : Model
init =
    Model initCampus ""

type alias Campus
    = List (Int, String)

initCampus : List (Int, String)
initCampus =
    (Array.toIndexedList (Array.fromList (List.repeat 256 "white")))

--UPDATE--
type Msg
    = ChangeColor Int String
    | ColorValue String

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeColor n color ->
            { model | campus = updateCampus model n color}

        ColorValue value ->
            { model | colorValue = value }
            

--VIEW--
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "SOSOGU" ]
        , p [] [ text "Pixel Art Editor with Elm" ]
        , div [] [ input [ (placeholder "Color"), (onInput ColorValue) ] [] ]
        , create8x8Campus model
        ]

--FUNC--
getCampusColor : Model -> Int -> String
getCampusColor model n =
    Tuple.second (Maybe.withDefault (-1, "_") (Array.get n (Array.fromList model.campus)))

getCampusInt : Model -> Int -> Int
getCampusInt model n =
    Tuple.first (Maybe.withDefault (1, "_") (Array.get n (Array.fromList model.campus)))

makeTable : Model -> Int -> Int -> Html Msg
makeTable model width height =
    div []
        [ table []
            <| List.map(\n -> tr[]
                <| List.map(\m -> td [] [])
                    <| List.range 1 width)
                        <| List.range 1 height ]

updateCampus : Model -> Int -> String -> List (Int, String)
updateCampus model n color =
    List.append (List.append (List.take n model.campus) (List.singleton (n, color))) (List.drop (n+1) model.campus)

create8x8Campus : Model -> Html Msg
create8x8Campus model =
    table []
        [ tr [] [ td [ onClick (ChangeColor 0 model.colorValue), style "background-color" (getCampusColor model 0) ] []
                , td [ onClick (ChangeColor 1 model.colorValue), style "background-color" (getCampusColor model 1) ] []
                , td [ onClick (ChangeColor 2 model.colorValue), style "background-color" (getCampusColor model 2) ] []
                , td [ onClick (ChangeColor 3 model.colorValue), style "background-color" (getCampusColor model 3) ] []
                , td [ onClick (ChangeColor 4 model.colorValue), style "background-color" (getCampusColor model 4) ] []
                , td [ onClick (ChangeColor 5 model.colorValue), style "background-color" (getCampusColor model 5) ] []
                , td [ onClick (ChangeColor 6 model.colorValue), style "background-color" (getCampusColor model 6) ] []
                , td [ onClick (ChangeColor 7 model.colorValue), style "background-color" (getCampusColor model 7) ] []
                ]
        , tr [] [ td [ onClick (ChangeColor 8 model.colorValue), style "background-color" (getCampusColor model 8) ] []
                , td [ onClick (ChangeColor 9 model.colorValue), style "background-color" (getCampusColor model 9) ] []
                , td [ onClick (ChangeColor 10 model.colorValue), style "background-color" (getCampusColor model 10) ] []
                , td [ onClick (ChangeColor 11 model.colorValue), style "background-color" (getCampusColor model 11) ] []
                , td [ onClick (ChangeColor 12 model.colorValue), style "background-color" (getCampusColor model 12) ] []
                , td [ onClick (ChangeColor 13 model.colorValue), style "background-color" (getCampusColor model 13) ] []
                , td [ onClick (ChangeColor 14 model.colorValue), style "background-color" (getCampusColor model 14) ] []
                , td [ onClick (ChangeColor 15 model.colorValue), style "background-color" (getCampusColor model 15) ] []
                ]
        , tr [] [ td [ onClick (ChangeColor 16 model.colorValue), style "background-color" (getCampusColor model 16) ] []
                , td [ onClick (ChangeColor 17 model.colorValue), style "background-color" (getCampusColor model 17) ] []
                , td [ onClick (ChangeColor 18 model.colorValue), style "background-color" (getCampusColor model 18) ] []
                , td [ onClick (ChangeColor 19 model.colorValue), style "background-color" (getCampusColor model 19) ] []
                , td [ onClick (ChangeColor 20 model.colorValue), style "background-color" (getCampusColor model 20) ] []
                , td [ onClick (ChangeColor 21 model.colorValue), style "background-color" (getCampusColor model 21) ] []
                , td [ onClick (ChangeColor 22 model.colorValue), style "background-color" (getCampusColor model 22) ] []
                , td [ onClick (ChangeColor 23 model.colorValue), style "background-color" (getCampusColor model 23) ] []
                ]
        , tr [] [ td [ onClick (ChangeColor 24 model.colorValue), style "background-color" (getCampusColor model 24) ] []
                , td [ onClick (ChangeColor 25 model.colorValue), style "background-color" (getCampusColor model 25) ] []
                , td [ onClick (ChangeColor 26 model.colorValue), style "background-color" (getCampusColor model 26) ] []
                , td [ onClick (ChangeColor 27 model.colorValue), style "background-color" (getCampusColor model 27) ] []
                , td [ onClick (ChangeColor 28 model.colorValue), style "background-color" (getCampusColor model 28) ] []
                , td [ onClick (ChangeColor 29 model.colorValue), style "background-color" (getCampusColor model 29) ] []
                , td [ onClick (ChangeColor 30 model.colorValue), style "background-color" (getCampusColor model 30) ] []
                , td [ onClick (ChangeColor 31 model.colorValue), style "background-color" (getCampusColor model 31) ] []
              ]
        , tr [] [ td [ onClick (ChangeColor 32 model.colorValue), style "background-color" (getCampusColor model 32) ] []
                , td [ onClick (ChangeColor 33 model.colorValue), style "background-color" (getCampusColor model 33) ] []
                , td [ onClick (ChangeColor 34 model.colorValue), style "background-color" (getCampusColor model 34) ] []
                , td [ onClick (ChangeColor 35 model.colorValue), style "background-color" (getCampusColor model 35) ] []
                , td [ onClick (ChangeColor 36 model.colorValue), style "background-color" (getCampusColor model 36) ] []
                , td [ onClick (ChangeColor 37 model.colorValue), style "background-color" (getCampusColor model 37) ] []
                , td [ onClick (ChangeColor 38 model.colorValue), style "background-color" (getCampusColor model 38) ] []
                , td [ onClick (ChangeColor 39 model.colorValue), style "background-color" (getCampusColor model 39) ] []
                ]
        , tr [] [ td [ onClick (ChangeColor 40 model.colorValue), style "background-color" (getCampusColor model 40) ] []
                , td [ onClick (ChangeColor 41 model.colorValue), style "background-color" (getCampusColor model 41) ] []
                , td [ onClick (ChangeColor 42 model.colorValue), style "background-color" (getCampusColor model 42) ] []
                , td [ onClick (ChangeColor 43 model.colorValue), style "background-color" (getCampusColor model 43) ] []
                , td [ onClick (ChangeColor 44 model.colorValue), style "background-color" (getCampusColor model 44) ] []
                , td [ onClick (ChangeColor 45 model.colorValue), style "background-color" (getCampusColor model 45) ] []
                , td [ onClick (ChangeColor 46 model.colorValue), style "background-color" (getCampusColor model 46) ] []
                , td [ onClick (ChangeColor 47 model.colorValue), style "background-color" (getCampusColor model 47) ] []
                ]
        , tr [] [ td [ onClick (ChangeColor 48 model.colorValue), style "background-color" (getCampusColor model 48) ] []
                , td [ onClick (ChangeColor 49 model.colorValue), style "background-color" (getCampusColor model 49) ] []
                , td [ onClick (ChangeColor 50 model.colorValue), style "background-color" (getCampusColor model 50) ] []
                , td [ onClick (ChangeColor 51 model.colorValue), style "background-color" (getCampusColor model 51) ] []
                , td [ onClick (ChangeColor 52 model.colorValue), style "background-color" (getCampusColor model 52) ] []
                , td [ onClick (ChangeColor 53 model.colorValue), style "background-color" (getCampusColor model 53) ] []
                , td [ onClick (ChangeColor 54 model.colorValue), style "background-color" (getCampusColor model 54) ] []
                , td [ onClick (ChangeColor 55 model.colorValue), style "background-color" (getCampusColor model 55) ] []
                ]
        , tr [] [ td [ onClick (ChangeColor 56 model.colorValue), style "background-color" (getCampusColor model 56) ] []
                , td [ onClick (ChangeColor 57 model.colorValue), style "background-color" (getCampusColor model 57) ] []
                , td [ onClick (ChangeColor 58 model.colorValue), style "background-color" (getCampusColor model 58) ] []
                , td [ onClick (ChangeColor 59 model.colorValue), style "background-color" (getCampusColor model 59) ] []
                , td [ onClick (ChangeColor 60 model.colorValue), style "background-color" (getCampusColor model 60) ] []
                , td [ onClick (ChangeColor 61 model.colorValue), style "background-color" (getCampusColor model 61) ] []
                , td [ onClick (ChangeColor 62 model.colorValue), style "background-color" (getCampusColor model 62) ] []
                , td [ onClick (ChangeColor 63 model.colorValue), style "background-color" (getCampusColor model 63) ] []
                ]
        ]
