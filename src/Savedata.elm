module Savedata exposing
    ( Savedata
    , isSavedata
    )

import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)


type alias Savedata =
    String


isSavedata : Savedata -> Bool
isSavedata savedata =
    case
        JD.decodeString (JD.field "general" (JD.field "isSOSOGUSavedata" JD.bool)) savedata
    of
        Ok bool ->
            bool

        Err _ ->
            False
