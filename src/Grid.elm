module Grid exposing
  ( Cell
  , Row
  , Grid
  , Width
  , Height
  , fromList
  , makeEmpty
  )

import Array

import ListUtil


type alias X = Int
type alias Y = Int
type alias Width = Int
type alias Height = Int
type alias Cell = Bool
type alias Row = Array.Array Cell
type alias Grid = Array.Array Row


fromList : Width -> Height -> List Cell -> Maybe Grid
fromList width height cells =
  if
    List.length cells == width * height
  then
    cells
      |> ListUtil.subdivideList width
      |> List.map Array.fromList
      |> Array.fromList
      |> Just
  else
    Nothing

emptyRow : Width -> Row
emptyRow width =
  Array.repeat width False

makeEmpty : Width -> Height -> Grid
makeEmpty width height = 
  Array.repeat height (emptyRow width)

setCellAt : X -> Y -> Grid -> Cell -> Grid
setCellAt x y grid isAlive =
  let
    setCellInRow =
      \row -> (Array.set y (Array.set x isAlive row) grid)
  in
    Array.get y grid
      |> Maybe.map setCellInRow
      |> Maybe.withDefault grid

flipCell : Cell -> Cell
flipCell = not

flipCellAt : X -> Y -> Grid -> Grid
flipCellAt x y grid =
  grid
    |> Array.get y
    |> Maybe.andThen (Array.get x)
    |> Maybe.map flipCell
    |> Maybe.map (setCellAt x y grid)
    |> Maybe.withDefault grid

