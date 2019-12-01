module GameOfLife exposing (main)

import Array
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Time

import Grid
import ListUtil



-- CONSTANTS

gridWidth : Int
gridWidth = 110

gridHeight : Int
gridHeight = 90


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type RunState
  = Running
  | Stopped

type alias Model =
    { grid : Grid.Grid
    , runState : RunState
    }

init : () -> (Model, Cmd Msg)
init _ = 
  ( { grid = Grid.makeEmpty gridWidth gridHeight
    , runState = Stopped
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewGridRequested ->
      ( model
      , Random.generate MakeGrid (Grid.random gridWidth gridHeight)
      )
    MakeGrid cells ->
      ( { grid = Grid.fromArray gridWidth cells
        , runState = Stopped
        }
      , Cmd.none
      )
    StartButtonPressed ->
        ( { model | runState = Running }
        , Cmd.none
        )
    StopButtonPressed ->
        ( { model | runState = Stopped }
        , Cmd.none
        )
    Tick _ ->
      case model.runState of
        Running ->
          ( { model | grid = Grid.evolve model.grid }
          , Cmd.none
          )
        Stopped ->
          (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.runState of
    Running ->
      Time.every 100 Tick
    Stopped ->
      Sub.none


-- VIEW

renderCell : Grid.Cell -> Html Msg
renderCell isAlive =
  let
    cellColor = if isAlive then "black" else "#EEE"
  in
    Html.td [ Attr.style "padding" "0.25em"
            , Attr.style "background-color" cellColor
            ] []

renderRow : List Grid.Cell -> Html Msg
renderRow row =
  Html.tr [] (List.map renderCell row)

toTable : Grid.Grid -> Html Msg
toTable grid =
  Html.table
    [ Attr.style "font-family" "monospace"
    , Attr.style "margin" "auto"
    ]
    ( grid.cells
      |> Array.toList
      |> ListUtil.subdivideList grid.width
      |> List.map renderRow
    )

view : Model -> Html Msg
view { grid, runState } =
  Html.div []
    [ Html.h1 [ Attr.style "text-align" "center" ]
      [ Html.text "Game of Life" ]
    , Html.div [ Attr.style "margin" "1.5em"
               , Attr.style "text-align" "center"]
      [ Html.button [ Attr.style "margin-right" "1ex"
                    , Events.onClick NewGridRequested
                    ]
        [ Html.text "New" ]
      , ( startStopButton runState )
      ]
    , Html.div [ Attr.style "margin" "auto" ]
      [ toTable grid ]
    ]

startStopButton : RunState -> Html Msg
startStopButton runState  = 
  case runState of
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
