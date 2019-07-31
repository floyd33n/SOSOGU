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
  { settingColorContent : String
  , masuX1Y1 : String
  , masuX1Y2 : String
  , masuX2Y1 : String
  , masuX2Y2 : String
  }

init : Model
init =
  Model "black" "white" "white" "white" "white"

--UPDATE--
type Msg
  = ChangeColorField String
  | PourX1Y1 String
  | PourX1Y2 String
  | PourX2Y1 String
  | PourX2Y2 String

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeColorField newsetcolor->
      { model | settingColorContent = newsetcolor }
    
    PourX1Y1 color ->
      { model | masuX1Y1 = color }

    PourX1Y2 color ->
      { model | masuX1Y2 = color }
      
    PourX2Y1 color ->
      { model | masuX2Y1 = color }
      
    PourX2Y2 color ->
      { model | masuX2Y2 = color }

--VIEW--
view : Model -> Html Msg
view model =
  div [ style "margin" "0"
      --, class "masudiv"
      ] 
    [ div []
        [ a [] [ text "Set Color: " ] 
        , input [ (placeholder "Color Field"), (onInput ChangeColorField), id "color" ] []
        , button [ style "background-color" model.settingColorContent
                 , style "height" "20px"
                 , style "width" "20px"
                 , style "border" "1px solid"
                 , style "padding" "0px"
                 , style "vertical-align" "middle"
                 ] []
        ]
    , div [] 
        [ table []
            [ tr [] [ td [] [ masuButton model PourX1Y1 model.masuX1Y1 ]
                    , td [] [ masuButton model PourX2Y1 model.masuX2Y1 ]
                    ]
            , tr [] [ td [] [ masuButton model PourX1Y2 model.masuX1Y2 ]
                    , td [] [ masuButton model PourX2Y2 model.masuX2Y2 ]
                    ]
            ]
        ]
    --, debugfunc model
    ]

--FUNC--
masuButton : Model -> (String -> Msg) -> String -> Html Msg
masuButton model pourpoint modelpoint =
  button [ onClick (pourpoint model.settingColorContent)
         , style "background-color" modelpoint
         , class "masu"
         --, style "height" "30px"
         --, style "width" "30px"
         --, style "border" "1px solid"
         ] []

  


--DEBUG--
{-
debugfunc : Model -> Html Msg
debugfunc model =
  div [] [ div [ style "color" model.settingColorContent ] [ text ("Inputed Color: " ++ model.settingColorContent) ]
         ]
-}
