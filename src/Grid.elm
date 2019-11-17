module Grid exposing
  ( Cell
  , Grid
  , fromList
  , makeEmpty
  , random
  , getCellAt
  , setCellAt
  , flipCellAt
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

toCoordinates : Int -> Grid -> (Int, Int)
toCoordinates index grid =
  ( modBy grid.width index 
  , index // grid.width
  )

getCellAt : (Int, Int) -> Grid -> Maybe Cell
getCellAt (x, y) grid =
  let
    index = toIndex (x, y) grid
  in
    Array.get index grid.cells

setCellAt : (Int, Int) -> Grid -> Cell -> Grid
setCellAt (x, y) grid isAlive =
  let
    index = toIndex (x, y) grid
    newCells = Array.set index isAlive grid.cells
  in
    Grid grid.width newCells

flipCell : Cell -> Cell
flipCell = not

flipCellAt : (Int, Int) -> Grid -> Grid
flipCellAt (x, y) grid =
  let
    cell =
      getCellAt (x, y) grid
  in
    cell
      |> Maybe.map flipCell
      |> Maybe.map (setCellAt (x, y) grid)
      |> Maybe.withDefault grid

