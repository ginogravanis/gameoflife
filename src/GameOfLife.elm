module GameOfLife exposing (main)

import Array
import Browser
import Grid
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import ListUtil
import Random
import Time



-- CONSTANTS


gridWidth : Int
gridWidth =
    110


gridHeight : Int
gridHeight =
    90



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type SimulationState
    = Running
    | Stopped


type alias Model =
    { grid : Grid.Grid
    , simulationState : SimulationState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = Grid.makeEmpty gridWidth gridHeight
      , simulationState = Stopped
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewGridRequested
    | MakeGrid (Array.Array Grid.Cell)
    | StartButtonPressed
    | StopButtonPressed
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGridRequested ->
            ( model
            , Random.generate MakeGrid (Grid.random gridWidth gridHeight)
            )

        MakeGrid cells ->
            ( { model | grid = Grid.fromArray gridWidth cells }
            , Cmd.none
            )

        StartButtonPressed ->
            ( { model | simulationState = Running }
            , Cmd.none
            )

        StopButtonPressed ->
            ( { model | simulationState = Stopped }
            , Cmd.none
            )

        Tick _ ->
            ( { model | grid = Grid.evolve model.grid }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.simulationState of
        Running ->
            Time.every 100 Tick

        Stopped ->
            Sub.none



-- VIEW


renderCell : Grid.Cell -> Html Msg
renderCell isAlive =
    let
        cellColor =
            if isAlive then
                "black"

            else
                "#EEE"
    in
    Html.td
        [ Attr.style "padding" "0.25em"
        , Attr.style "background-color" cellColor
        ]
        []


renderRow : List Grid.Cell -> Html Msg
renderRow row =
    Html.tr [] (List.map renderCell row)


toTable : Grid.Grid -> Html Msg
toTable grid =
    Html.table
        [ Attr.style "font-family" "monospace"
        , Attr.style "margin" "auto"
        ]
        (grid.cells
            |> Array.toList
            |> ListUtil.subdivideList grid.width
            |> List.map renderRow
        )


view : Model -> Html Msg
view { grid, simulationState } =
    Html.div []
        [ Html.h1 [ Attr.style "text-align" "center" ]
            [ Html.text "Game of Life" ]
        , Html.div
            [ Attr.style "margin" "1.5em"
            , Attr.style "text-align" "center"
            ]
            [ Html.button
                [ Attr.style "margin-right" "1ex"
                , Events.onClick NewGridRequested
                ]
                [ Html.text "New" ]
            , startStopButton simulationState
            ]
        , Html.div [ Attr.style "margin" "auto" ]
            [ toTable grid ]
        ]


startStopButton : SimulationState -> Html Msg
startStopButton simulationState =
    case simulationState of
        Stopped ->
            startButton

        Running ->
            stopButton


startButton : Html Msg
startButton =
    Html.button [ Events.onClick StartButtonPressed ]
        [ Html.text "Start" ]


stopButton : Html Msg
stopButton =
    Html.button [ Events.onClick StopButtonPressed ]
        [ Html.text "Stop" ]
