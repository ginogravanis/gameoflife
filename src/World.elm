module World exposing
    ( Cell
    , Model
    , evolve
    , fromArray
    , fromList
    , makeEmpty
    , random
    )

import Array
import ListUtil
import Random
import Random.Array
import Random.Extra


type alias Cell =
    Bool


type alias Model =
    { width : Int
    , cells : Array.Array Cell
    }


fromArray : Int -> Array.Array Cell -> Model
fromArray =
    Model


fromList : Int -> List Cell -> Model
fromList width cells =
    fromArray width (Array.fromList cells)


makeEmpty : Int -> Int -> Model
makeEmpty width height =
    let
        cellCount =
            width * height
    in
    Model width (Array.repeat cellCount False)


random : Int -> Int -> Random.Generator (Array.Array Bool)
random width height =
    let
        count =
            width * height
    in
    Random.Array.array count Random.Extra.bool


toIndex : ( Int, Int ) -> Model -> Int
toIndex ( x, y ) model =
    (y * model.width) + x


toCoordinates : Model -> Int -> ( Int, Int )
toCoordinates model index =
    ( modBy model.width index
    , index // model.width
    )


getCellAt : Model -> ( Int, Int ) -> Maybe Cell
getCellAt model ( x, y ) =
    let
        index =
            toIndex ( x, y ) model
    in
    Array.get index model.cells


setCellAt : Model -> ( Int, Int ) -> Cell -> Model
setCellAt model ( x, y ) isAlive =
    let
        index =
            toIndex ( x, y ) model

        newCells =
            Array.set index isAlive model.cells
    in
    Model model.width newCells


flipCell : Cell -> Cell
flipCell =
    not


flipCellAt : Model -> ( Int, Int ) -> Model
flipCellAt model ( x, y ) =
    let
        cell =
            getCellAt model ( x, y )
    in
    cell
        |> Maybe.map flipCell
        |> Maybe.map (setCellAt model ( x, y ))
        |> Maybe.withDefault model


evolve : Model -> Model
evolve model =
    let
        toWorldCoords =
            toCoordinates model

        enumeratedCells =
            Array.toIndexedList model.cells

        evolvehelp =
            \( index, cell ) ->
                evolveCell model (toWorldCoords index) cell

        newCells =
            List.map evolvehelp enumeratedCells
    in
    fromList model.width newCells


evolveCell : Model -> ( Int, Int ) -> Cell -> Cell
evolveCell model ( x, y ) cell =
    let
        aliveNeighbours =
            liveNeighbours model ( x, y )
    in
    if aliveNeighbours == 3 then
        True

    else if cell && aliveNeighbours == 2 then
        True

    else
        False


liveNeighbours : Model -> ( Int, Int ) -> Int
liveNeighbours model ( x, y ) =
    let
        neighbourCoords =
            [ ( x, y - 1 )
            , ( x + 1, y - 1 )
            , ( x + 1, y )
            , ( x + 1, y + 1 )
            , ( x, y + 1 )
            , ( x - 1, y + 1 )
            , ( x - 1, y )
            , ( x - 1, y - 1 )
            ]

        toInt cell =
            case cell of
                True ->
                    1

                False ->
                    0
    in
    neighbourCoords
        |> List.map (getCellAt model)
        |> List.map (Maybe.withDefault False)
        |> List.map toInt
        |> List.sum
