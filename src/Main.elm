module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (maximumBy)
import SharedModels exposing (GMPos)
import GMaps exposing (moveMap, addMarker, mapMoved)


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init testDevices testReports
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { devices : List Device
    , deviceId : Maybe Int
    , name : String
    , reports : List Report
    , mapPos : GMPos
    }



-- UPDATE


type Msg
    = DeviceVisible Device
    | ReportVisible Report
    | LastLocation Device
    | History Device
    | MapMoved GMPos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeviceVisible device ->
            ( editDevice model device, Cmd.none )

        ReportVisible report ->
            ( editReport model report, addMarker report.location )

        History device ->
            ( { model
                | deviceId = Just device.id
                , name = device.identifier
              }
            , Cmd.none
            )

        LastLocation device ->
            let
                lastReport =
                    model.reports
                        |> List.filter (\rpt -> rpt.deviceId == device.id)
                        |> List.Extra.maximumBy .id
            in
                case lastReport of
                    Just report ->
                        ( model, moveMap report.location )

                    Nothing ->
                        ( model, Cmd.none )

        MapMoved newPos ->
            ( { model | mapPos = newPos }
            , Cmd.none
            )


editReport : Model -> Report -> Model
editReport model report =
    let
        newReports =
            model.reports
                |> List.map
                    (\rpt ->
                        if rpt.id == report.id then
                            { rpt | visible = not rpt.visible }
                        else
                            rpt
                    )
    in
        { model | reports = newReports }


editDevice : Model -> Device -> Model
editDevice model device =
    let
        newDevices =
            model.devices
                |> List.map
                    (\d ->
                        if d.id == device.id then
                            { d | visible = not d.visible }
                        else
                            d
                    )
    in
        { model | devices = newDevices }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "ElmElite" ]
        , deviceSection model
        , reportSection model
        , p [] [ text (toString model) ] -- TODO: Remove Debug
        ]


reportSection : Model -> Html Msg
reportSection model =
    div []
        [ reportListHeader model
        , reportList model
        ]


reportListHeader : Model -> Html Msg
reportListHeader model =
    header []
        [ div [] [ text model.name ]
        , div [] [ text "Report Date" ]
        , div [] [ text "Latitude" ]
        , div [] [ text "Longitude" ]
        ]


reportList : Model -> Html Msg
reportList model =
    case model.deviceId of
        Just deviceId ->
            model.reports
                |> List.filter (\report -> report.deviceId == deviceId)
                |> List.sortBy .id
                |> List.map report
                |> ul []

        Nothing ->
            ul [] []


report : Report -> Html Msg
report report =
    li []
        [ input
            [ type_ "checkbox"
            , onClick (ReportVisible report)
            , checked report.visible
            ]
            []
        , div []
            [ text report.reportDate ]
        , div []
            [ text (toString report.location.lat) ]
        , div []
            [ text (toString report.location.lng) ]
        ]


deviceSection : Model -> Html Msg
deviceSection model =
    div []
        [ deviceListHeader
        , deviceList model
        ]


deviceListHeader : Html Msg
deviceListHeader =
    header []
        [ div [] [ text "Devices" ] ]


deviceList : Model -> Html Msg
deviceList model =
    model.devices
        |> List.sortBy .identifier
        |> List.map device
        |> ul []


device : Device -> Html Msg
device device =
    li []
        [ input [ type_ "checkbox", onClick (DeviceVisible device) ]
            []
        , div []
            [ text device.identifier ]
        , button
            [ type_ "button"
            , onClick (LastLocation device)
            ]
            [ text "Find" ]
        , button
            [ type_ "button"
            , onClick (History device)
            ]
            [ text "History" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    mapMoved MapMoved



-- INIT


init : List Device -> List Report -> ( Model, Cmd Msg )
init devices reports =
    let
        model =
            { devices = devices
            , deviceId = Nothing
            , name = ""
            , reports = reports
            , mapPos = (GMPos 48.2206636 16.3100206)
            }
    in
        ( model, Cmd.none )



-- DEVICES AND REPORTS


type alias Device =
    { id : Int
    , identifier : String
    , visible : Bool
    , icon : String
    }


type alias Report =
    { id : Int
    , deviceId : Int
    , reportDate : String
    , location : GMPos
    , visible : Bool
    }


testDevices : List Device
testDevices =
    [ Device 0 "st4k:840A-4" False ""
    , Device 1 "sm3k:123456789" False ""
    , Device 2 "sm3k:99000226866081" False ""
    ]


testReports : List Report
testReports =
    [ Report 0 0 "05/04/2017 03:48:34" (GMPos 53.78052 -1.56265) False
    , Report 1 0 "05/04/2017 03:48:44" (GMPos 53.78139 -1.56055) False
    , Report 2 0 "05/04/2017 03:48:54" (GMPos 53.78237 -1.55865) False
    , Report 3 0 "05/04/2017 03:49:04" (GMPos 53.78317 -1.55655) False
    , Report 4 0 "05/04/2017 03:49:14" (GMPos 53.78382 -1.55422) False
    , Report 5 1 "02/17/2017 13:18:59" (GMPos 27.85297 -82.68534) False
    , Report 6 1 "02/17/2017 13:20:03" (GMPos 27.85314 -82.68562) False
    , Report 7 1 "02/17/2017 13:21:01" (GMPos 27.85322 -82.68523) False
    , Report 8 1 "02/17/2017 13:22:02" (GMPos 27.85308 -82.68598) False
    ]
