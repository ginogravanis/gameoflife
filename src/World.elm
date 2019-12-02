module World exposing
    ( Cell(..)
    , Model
    , evolve
    , fromArray
    , fromBoolArray
    , fromList
    , makeEmpty
    , random
    )

import Array
import ListUtil
import Random
import Random.Array
import Random.Extra


type Cell
    = Dead
    | Alive


type alias Model =
    { width : Int
    , cells : Array.Array Cell
    }


fromBool : Bool -> Cell
fromBool isAlive =
    if isAlive then
        Alive

    else
        Dead


fromBoolArray : Int -> Array.Array Bool -> Model
fromBoolArray width bools =
    fromArray width (Array.map fromBool bools)


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
    Model width (Array.repeat cellCount Dead)


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
setCellAt model ( x, y ) cell =
    let
        index =
            toIndex ( x, y ) model

        newCells =
            Array.set index cell model.cells
    in
    Model model.width newCells


flipCell : Cell -> Cell
flipCell cell =
    case cell of
        Dead ->
            Alive

        Alive ->
            Dead


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
        Alive

    else if (cell == Alive) && aliveNeighbours == 2 then
        Alive

    else
        Dead


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
                Alive ->
                    1

                Dead ->
                    0
    in
    neighbourCoords
        |> List.map (getCellAt model)
        |> List.map (Maybe.withDefault Dead)
        |> List.map toInt
        |> List.sum
