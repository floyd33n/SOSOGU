module Main exposing(main)
import Browser
import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Events exposing (..)
import Array exposing (..)
import List.Extra

main =
  Browser.sandbox { init = init, update = update, view = view }

--MODEL--
type alias Model = 
  { campus : Campus
  }

init : Model
init =
    Model initCampus

type alias Campus
    = List (Int, String)

initCampus : List (Int, String)
initCampus =
    (Array.toIndexedList (Array.fromList (List.repeat 256 "white")))

--UPDATE--
type Msg
    = ChangeColor Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeColor n color ->
            { model | campus = updateCampus model n color }


--VIEW--
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Pour-Masu" ]
        , p [] [ text "Pixel Art Editor with Elm" ]
        , table [] [ tr [] [ td [ (onClick (ChangeColor 0 "red")), (style "background-color" (getCampusColor model 0)) ] []
                           , td [ (onClick (ChangeColor 1 "pink")), (style "background-color" (getCampusColor model 1)) ] []
                           ]
                   , tr [] [ td [ (onClick (ChangeColor 2 "green")), (style "background-color" (getCampusColor model 2)) ] []
                           , td [ (onClick (ChangeColor 3 "blue")), style "background-color" (getCampusColor model 3) ] []
                           ]
                   ]
        , chkFunc model
        ]

--FUNC--
getCampusColor : Model -> Int -> String
getCampusColor model n =
    Tuple.second (Maybe.withDefault (-1, "_") (Array.get n (Array.fromList model.campus)))
{-
updateCampus : Model ->  Int -> String -> List (Int, String)
updateCampus model n color =
    let
        firstTemp : List (Int, String) -> List (Int, String)
        firstTemp temp =
            List.take (n-1) model.campus

        updateTemp : Int -> String -> List (Int, String)
        updateTemp tempn tempcolor =
            List.singleton ((n-1), color)

        secondTemp : List (Int, String) -> List (Int, String)
        secondTemp temp =
            List.drop n model.campus


        appendTemps : List (Int, String) -> List (Int, String)
        appendTemps temp =
            List.append (List.append (firstTemp model.campus) (updateTemp n color)) (secondTemp model.campus)
    in
        model.campus |> appendTemps
-}
updateCampus : Model -> Int -> String -> List (Int, String)
updateCampus model n color =
    List.append (List.append (List.take n model.campus) (List.singleton (n, color))) (List.drop (n+1) model.campus)

--DEBUG--
chkFunc : Model -> Html Msg
chkFunc model =
    div []
        [ h3 [] [ text "--Debug--" ]
        , button [ onClick (ChangeColor 3 "red")  ] [ text "red" ]
        , p [ style "color" (getCampusColor model 2) ] [ text "What Color" ] 
        ]
