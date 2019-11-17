module Grid exposing
  ( Cell
  , Grid
  , fromList
  , makeEmpty
  , random
  , evolve
  )

import Array
import Random.Extra

import ListUtil


type alias Cell = Bool
type alias Grid =
  { width : Int
  , cells : Array.Array Cell
  }


fromArray : Int -> Array.Array Cell -> Grid
fromArray = Grid

fromList : Int -> List Cell -> Grid
fromList width cells =
  fromArray width (Array.fromList cells)

makeEmpty : Int -> Int -> Grid
makeEmpty width height = 
  let
    cellCount = width * height
  in
    Grid width (Array.repeat cellCount False)

random width height =
  Random.Extra.bool
    |> List.repeat (width * height)
    |> Random.Extra.sequence

toIndex : (Int, Int) -> Grid -> Int
toIndex (x, y) grid =
  (y * grid.width) + x

toCoordinates : Grid -> Int -> (Int, Int)
toCoordinates grid index =
  ( modBy grid.width index 
  , index // grid.width
  )

getCellAt : Grid -> (Int, Int) -> Maybe Cell
getCellAt grid (x, y) =
  let
    index = toIndex (x, y) grid
  in
    Array.get index grid.cells

setCellAt : Grid -> (Int, Int) -> Cell -> Grid
setCellAt grid (x, y) isAlive =
  let
    index = toIndex (x, y) grid
    newCells = Array.set index isAlive grid.cells
  in
    Grid grid.width newCells

flipCell : Cell -> Cell
flipCell = not

flipCellAt : Grid -> (Int, Int) -> Grid
flipCellAt grid (x, y) =
  let
    cell =
      getCellAt grid (x, y)
  in
    cell
      |> Maybe.map flipCell
      |> Maybe.map (setCellAt grid (x, y))
      |> Maybe.withDefault grid

evolve : Grid -> Grid
evolve grid =
  let
    toGridCoords =
      toCoordinates grid

    enumeratedCells =
      Array.toIndexedList grid.cells

    evolvehelp = (
      \(index, cell) ->
        evolveCell grid (toGridCoords index) cell)

    newCells =
      List.map evolvehelp enumeratedCells
  in
    fromList grid.width newCells

evolveCell : Grid -> (Int, Int) -> Cell -> Cell
evolveCell grid (x, y) cell =
  let
    aliveNeighbours =
      liveNeighbours grid (x, y)
  in
    if aliveNeighbours == 3
      then True
    else if cell && aliveNeighbours == 2
      then True
    else
      False

liveNeighbours : Grid -> (Int, Int) -> Int
liveNeighbours grid (x, y) =
  let
    neighbourCoords =
      [ (x, y - 1)
      , (x + 1, y - 1)
      , (x + 1, y)
      , (x + 1, y + 1)
      , (x, y + 1)
      , (x - 1, y + 1)
      , (x - 1, y)
      , (x - 1, y - 1)
      ]

    toInt cell =
      case cell of
        True ->
          1
        False ->
          0
  in
    neighbourCoords
      |> List.map (getCellAt grid)
      |> List.map (Maybe.withDefault False)
      |> List.map toInt
      |> List.sum
