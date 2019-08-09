module Main exposing(main)

import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Array exposing (..)
import Debug exposing (..)

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
    = List (List (Int, String))

initCampus : List (List (Int, String))
initCampus =
    List.repeat (8) (Array.toIndexedList (Array.fromList (List.repeat (8) "white")))

--UPDATE--
type Msg
    = ChangeColor Int Int String
    | ColorValue String

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeColor x y color ->
            { model | campus = updateCampus model x y color}

        ColorValue value ->
            { model | colorValue = value }
            

--VIEW--
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "SOSOGU" ]
        , p [] [ text "Pixel Art Editor with Elm" ]
        , div [] [ input [ (placeholder "Color"), (onInput ColorValue) ] [] ]
        , makeTable model 8 8
        ]

--FUNC--
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
                <| List.map(\x -> td [onClick(ChangeColor y x model.colorValue), style "background-color" (getCampusColor model y x)] [ text ((String.fromInt y) ++ "," ++ (String.fromInt x)) ])
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
