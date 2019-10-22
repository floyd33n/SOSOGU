module Savedata exposing (..)

import Base64 exposing (..)
import Bytes exposing (..)
import Bytes.Decode as BD exposing (..)
import Bytes.Encode as BE exposing (..)
import Dict exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE exposing (..)
import List.Extra as ListEx exposing (..)
import Types exposing (..)
import Utilities exposing (..)


isSavedata : Savedata -> Bool
isSavedata savedata =
    case
        JD.decodeString
            (JD.field "general" <| JD.field "isSOSOGUSavedata" JD.bool)
        <|
            savedata
    of
        Ok bool ->
            bool

        Err _ ->
            False


createSavedata : Model -> String
createSavedata model =
    let
        campusData : List String
        campusData =
            List.concat <|
                List.map
                    (\x ->
                        List.map
                            (\y ->
                                "("
                                    ++ String.fromInt x
                                    ++ ","
                                    ++ String.fromInt y
                                    ++ "),"
                                    ++ getCampusColor model.campus ( x, y )
                            )
                        <|
                            List.range 0 (model.campusSize.width - 1)
                    )
                <|
                    List.range 0 (model.campusSize.height - 1)

        historyData : List String
        historyData =
            let
                historyValue key =
                    Maybe.withDefault ( ( 0, 0 ), "white" ) <|
                        Dict.get (key - 1) model.history

                historyX v =
                    Tuple.first <| Tuple.first v

                historyY v =
                    Tuple.second <| Tuple.first v

                historyColor v =
                    Tuple.second v
            in
            List.map
                (\n ->
                    String.fromInt n
                        ++ ",("
                        ++ String.fromInt (historyX (historyValue n))
                        ++ ","
                        ++ String.fromInt (historyY (historyValue n))
                        ++ "),"
                        ++ historyColor (historyValue n)
                )
            <|
                List.range 0 (Dict.size model.history)

        subPaletteData : List String
        subPaletteData =
            List.map
                (\n ->
                    String.fromInt n
                        ++ ","
                        ++ getSubPaletteColor model.subPalette n
                )
            <|
                List.range 0 (Dict.size model.subPalette - 1)

        aEn =
            ( ( JE.int, JE.int ), JE.string )
    in
    JE.encode 4 <|
        JE.object
            [ ( "general"
              , JE.object
                    [ ( "isSOSOGUSavedata", JE.bool True )
                    , ( "version", JE.string "1.0" )
                    ]
              )
            , ( "setting"
              , JE.object
                    [ ( "borderColor", JE.string model.setting.borderColor )
                    , ( "borderStyle", JE.string model.setting.borderStyle )
                    , ( "width", JE.string model.setting.width )
                    , ( "height", JE.string model.setting.height )
                    , ( "panelPosition"
                      , JE.object
                            [ ( "settingPanel"
                              , JE.string <|
                                    case model.setting.panelPosition.settingPanel of
                                        Right ->
                                            "Right"

                                        Left ->
                                            "Left"
                              )
                            , ( "palettePanel"
                              , JE.string <|
                                    case model.setting.panelPosition.palettePanel of
                                        Right ->
                                            "Right"

                                        Left ->
                                            "Left"
                              )
                            , ( "campusPanel"
                              , JE.string <|
                                    case model.setting.panelPosition.campusPanel of
                                        TopCenter ->
                                            "TopCenter"

                                        TopRight ->
                                            "TopRight"

                                        TopLeft ->
                                            "TopLeft"
                              )
                            ]
                      )
                    ]
              )
            , ( "campusSize"
              , JE.object
                    [ ( "width", JE.int model.campusSize.width )
                    , ( "height", JE.int model.campusSize.height )
                    ]
              )
            , ( "campus", JE.list JE.string campusData )
            , ( "didCreateCampus", JE.bool model.didCreateCampus )
            , ( "mainPalette", JE.string model.mainPalette )
            , ( "subPalette", JE.list JE.string subPaletteData )
            , ( "history", JE.list JE.string historyData )
            ]


decodeDidCreateCampus : Savedata -> Bool
decodeDidCreateCampus savedata =
    case
        JD.decodeString
            (JD.field "didCreateCampus" JD.bool)
        <|
            savedata
    of
        Ok bool ->
            bool

        Err _ ->
            False


decodeCampusSize : Savedata -> CampusSize
decodeCampusSize savedata =
    { width =
        case
            JD.decodeString
                (JD.field "campusSize" <| JD.field "width" JD.int)
            <|
                savedata
        of
            Ok width ->
                width

            Err _ ->
                8
    , height =
        case
            JD.decodeString
                (JD.field "campusSize" <| JD.field "height" JD.int)
            <|
                savedata
        of
            Ok height ->
                height

            Err _ ->
                8
    }


decodeMainPalette : Savedata -> CssColor
decodeMainPalette savedata =
    case
        JD.decodeString
            (JD.field "mainPalette" JD.string)
        <|
            savedata
    of
        Ok color ->
            color

        Err _ ->
            "white"


decodeSetting : Savedata -> Setting
decodeSetting savedata =
    let
        decodeBorderColor : CssColor
        decodeBorderColor =
            case
                JD.decodeString
                    (JD.field "setting" <| JD.field "borderColor" JD.string)
                <|
                    savedata
            of
                Ok color ->
                    color

                Err _ ->
                    "black"

        decodeBorderStyle : String
        decodeBorderStyle =
            case
                JD.decodeString
                    (JD.field "setting" <| JD.field "borderStyle" JD.string)
                <|
                    savedata
            of
                Ok style ->
                    style

                Err _ ->
                    "solid"

        decodeWidth : String
        decodeWidth =
            case
                JD.decodeString
                    (JD.field "setting" <| JD.field "width" JD.string)
                <|
                    savedata
            of
                Ok width ->
                    width

                Err _ ->
                    "20"

        decodeHeight : String
        decodeHeight =
            case
                JD.decodeString
                    (JD.field "setting" <| JD.field "height" JD.string)
                <|
                    savedata
            of
                Ok height ->
                    height

                Err _ ->
                    "20"

        decodePanelPosition : PanelPosition
        decodePanelPosition =
            { settingPanel =
                case
                    JD.decodeString
                        (JD.field "setting" <| JD.field "panelPosition" <| JD.field "settingPanel" JD.string)
                    <|
                        savedata
                of
                    Ok settingPanel ->
                        case settingPanel of
                            "Right" ->
                                Right

                            "Left" ->
                                Left

                            _ ->
                                Left

                    Err _ ->
                        Left
            , palettePanel =
                case
                    JD.decodeString
                        (JD.field "setting" <| JD.field "panelPosition" <| JD.field "palettePanel" JD.string)
                    <|
                        savedata
                of
                    Ok palettePanel ->
                        case palettePanel of
                            "Right" ->
                                Right

                            "Left" ->
                                Left

                            _ ->
                                Right

                    Err _ ->
                        Right
            , campusPanel =
                case
                    JD.decodeString
                        (JD.field "setting" <| JD.field "panelPosition" <| JD.field "campusPanel" JD.string)
                    <|
                        savedata
                of
                    Ok campusPanel ->
                        case campusPanel of
                            "TopCenter" ->
                                TopCenter

                            "TopRight" ->
                                TopRight

                            "TopLeft" ->
                                TopLeft

                            _ ->
                                TopCenter

                    Err _ ->
                        TopCenter
            }
    in
    { borderColor = decodeBorderColor
    , borderStyle = decodeBorderStyle
    , width = decodeWidth
    , height = decodeHeight
    , panelPosition = decodePanelPosition
    }


decodeHistory : Savedata -> History
decodeHistory savedata =
    let
        decodeHistoryField : List String
        decodeHistoryField =
            case
                JD.decodeString
                    (JD.field "history" <| JD.list JD.string)
                <|
                    savedata
            of
                Ok history ->
                    history

                Err _ ->
                    []

        getSerial n historyData =
            --Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (ListEx.getAt 0 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData))))))
            historyData
                |> ListEx.getAt n
                |> Maybe.withDefault "0,(0,0),white"
                |> String.split ","
                |> ListEx.getAt 0
                |> Maybe.withDefault "0"
                |> String.toInt
                |> Maybe.withDefault 0

        getPoint n historyData =
            let
                getX =
                    --Maybe.withDefault 0 (String.toInt (String.dropLeft 1 (Maybe.withDefault "(0" (ListEx.getAt 1 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData)))))))
                    historyData
                        |> ListEx.getAt n
                        |> Maybe.withDefault "0,(0,0),white"
                        |> String.split ","
                        |> ListEx.getAt 1
                        |> Maybe.withDefault "(0"
                        |> String.dropLeft 1
                        |> String.toInt
                        |> Maybe.withDefault 0

                getY =
                    --Maybe.withDefault 0 (String.toInt (String.dropRight 1 (Maybe.withDefault "0)" (ListEx.getAt 2 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData)))))))
                    historyData
                        |> ListEx.getAt n
                        |> Maybe.withDefault "0,(0,0),white"
                        |> String.split ","
                        |> ListEx.getAt 2
                        |> Maybe.withDefault "0)"
                        |> String.dropRight 1
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            ( getX, getY )

        getColor n historyData =
            --Maybe.withDefault "white" (ListEx.getAt 3 (String.split "," (Maybe.withDefault "0,(0,0),white" (ListEx.getAt n historyData))))
            historyData
                |> ListEx.getAt n
                |> Maybe.withDefault "0,(0,0),white"
                |> String.split ","
                |> ListEx.getAt 3
                |> Maybe.withDefault "white"

        makeHistory n historyData =
            Tuple.pair
                (getSerial n historyData)
                (Tuple.pair
                    (getPoint n historyData)
                    (getColor n historyData)
                )
    in
    Dict.fromList <|
        List.map (\n -> makeHistory n decodeHistoryField) <|
            List.range 0 (List.length decodeHistoryField - 1)


decodeSubPalette : Savedata -> SubPalette
decodeSubPalette savedata =
    let
        decodeSubPaletteField : List String
        decodeSubPaletteField =
            case
                JD.decodeString
                    (JD.field "subPalette" <| JD.list JD.string)
                <|
                    savedata
            of
                Ok subPalette ->
                    subPalette

                Err _ ->
                    []

        getSerial n subPaletteData =
            subPaletteData
                |> ListEx.getAt n
                |> Maybe.withDefault "0,white"
                |> String.split ","
                |> ListEx.getAt 0
                |> Maybe.withDefault "0"
                |> String.toInt
                |> Maybe.withDefault 0

        getColor n subPaletteData =
            --Maybe.withDefault "white" <| ListEx.getAt 1 <| String.split "," <| Maybe.withDefault "0,white" <| ListEx.getAt n subPaletteData
            subPaletteData
                |> ListEx.getAt n
                |> Maybe.withDefault "0,white"
                |> String.split ","
                |> ListEx.getAt 1
                |> Maybe.withDefault "white"

        makeSubPalette n subPaletteData =
            Tuple.pair
                (getSerial n subPaletteData)
                (getColor n subPaletteData)
    in
    Dict.fromList <|
        List.map (\n -> makeSubPalette n decodeSubPaletteField) <|
            List.range 0 (List.length decodeSubPaletteField - 1)


decodeCampus : Savedata -> Campus
decodeCampus savedata =
    let
        decodeCampusField : List String
        decodeCampusField =
            case
                JD.decodeString
                    (JD.field "campus" <| JD.list JD.string)
                <|
                    savedata
            of
                Ok campus ->
                    campus

                Err _ ->
                    []

        getPoint : Int -> List String -> Point
        getPoint n campusData =
            let
                getX : Int
                getX =
                    --Maybe.withDefault 0 (String.toInt (String.slice 1 2 (Maybe.withDefault "0" (ListEx.getAt n campusData))))
                    campusData
                        |> ListEx.getAt n
                        |> Maybe.withDefault "0"
                        |> String.slice 1 2
                        |> String.toInt
                        |> Maybe.withDefault 0

                getY : Int
                getY =
                    --Maybe.withDefault 0 (String.toInt (String.slice 3 4 (Maybe.withDefault "0" (ListEx.getAt n campusData))))
                    campusData
                        |> ListEx.getAt n
                        |> Maybe.withDefault "0"
                        |> String.slice 3 4
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            ( getX, getY )

        getColor : Int -> List String -> CssColor
        getColor n campusData =
            --Maybe.withDefault "black" (ListEx.getAt 2 (String.split "," (Maybe.withDefault "white" (ListEx.getAt n campusData))))
            campusData
                |> ListEx.getAt n
                |> Maybe.withDefault "white"
                |> String.split ","
                |> ListEx.getAt 2
                |> Maybe.withDefault "black"

        makeCampusList : Int -> List String -> ( Point, CssColor )
        makeCampusList n campusData =
            Tuple.pair
                (getPoint n campusData)
                (getColor n campusData)
    in
    Dict.fromList <|
        List.map (\n -> makeCampusList n decodeCampusField) <|
            List.range 0 (List.length decodeCampusField - 1)


encodeSavedataWithBase64 model =
    createSavedata model
        |> BE.string
        |> BE.encode
        |> Base64.fromBytes
        |> Maybe.withDefault ""


decodeSavedata savedata =
    let
        dummy =
            BE.encode (BE.string "")

        b64ToBytes =
            Maybe.withDefault dummy (Base64.toBytes savedata)
    in
    --Maybe.withDefault "" <|
    --    BD.decode (BD.string <| Bytes.width b64ToBytes) b64ToBytes
    b64ToBytes
        |> BD.decode (BD.string <| Bytes.width b64ToBytes)
        |> Maybe.withDefault ""
