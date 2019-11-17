module GameOfLife exposing (main)

import Array
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Random

import Grid
import ListUtil



-- CONSTANTS

gridWidth : Int
gridWidth = 60

gridHeight : Int
gridHeight = 60


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

init : () -> (Grid.Grid, Cmd Msg)
init _ = 
  ( Grid.makeEmpty gridWidth gridHeight
  , Cmd.none
  )


-- UPDATE

type Msg
  = NewGridRequested
  | MakeGrid (List Grid.Cell)
  | Tick

update : Msg -> Grid.Grid -> (Grid.Grid, Cmd Msg)
update msg grid =
  case msg of
    NewGridRequested ->
      ( grid
      , Random.generate MakeGrid (Grid.random gridWidth gridHeight)
      )
    MakeGrid cells ->
      (Grid.fromList gridWidth cells, Cmd.none)
    Tick ->
      (grid, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Grid.Grid -> Sub Msg
subscriptions model =
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

view : Grid.Grid -> Html Msg
view grid =
  Html.div []
    [ Html.h1 [ Attr.style "text-align" "center" ]
      [ Html.text "Game of Life" ]
    , Html.div [ Attr.style "margin" "1.5em"
               , Attr.style "text-align" "center"]
      [ Html.button [ Attr.style "margin-right" "1ex"
                    , Events.onClick NewGridRequested
                    ]
        [ Html.text "New" ]
      , Html.button [ Events.onClick Tick ]
        [ Html.text "Tick" ]
      ]
    , Html.div [ Attr.style "margin" "auto" ]
      [ toTable grid ]
    ]
