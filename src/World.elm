module World exposing
    ( Cell(..)
    , Model
    , Msg(..)
    , evolve
    , fromBools
    , init
    , update
    )

import Array
import ListUtil
import Random
import Random.Array
import Random.Extra
import Time



-- Model


type alias Model =
    { width : Int
    , cells : Array.Array Cell
    }


type Msg
    = NewRequested
    | MakeWorld (Array.Array Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRequested ->
            ( model
            , Random.generate MakeWorld (randomWorld (dimensions model))
            )

        MakeWorld bools ->
            ( fromBools model.width bools
            , Cmd.none
            )


fromBools : Int -> Array.Array Bool -> Model
fromBools width bools =
    Model width (Array.map fromBool bools)


fromCells : Int -> List Cell -> Model
fromCells width cells =
    Model width (Array.fromList cells)


init : ( Int, Int ) -> Model
init ( width, height ) =
    let
        cellCount =
            width * height
    in
    Model width (Array.repeat cellCount Dead)


dimensions : Model -> ( Int, Int )
dimensions model =
    ( model.width
    , Array.length model.cells // model.width
    )


randomWorld : ( Int, Int ) -> Random.Generator (Array.Array Bool)
randomWorld ( width, height ) =
    let
        count =
            width * height
    in
    Random.Array.array count Random.Extra.bool


getCellAt : Model -> Coords -> Maybe Cell
getCellAt model coords =
    let
        index =
            toIndex coords model
    in
    Array.get index model.cells


setCellAt : Model -> Coords -> Cell -> Model
setCellAt model coords cell =
    let
        index =
            toIndex coords model

        newCells =
            Array.set index cell model.cells
    in
    Model model.width newCells


flipCellAt : Model -> Coords -> Model
flipCellAt model coords =
    let
        cell =
            getCellAt model coords
    in
    cell
        |> Maybe.map flipCell
        |> Maybe.map (setCellAt model coords)
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
    fromCells model.width newCells


evolveCell : Model -> Coords -> Cell -> Cell
evolveCell model coords cell =
    let
        aliveNeighbours =
            liveNeighbours model coords
    in
    if aliveNeighbours == 3 then
        Alive

    else if (cell == Alive) && aliveNeighbours == 2 then
        Alive

    else
        Dead


liveNeighbours : Model -> Coords -> Int
liveNeighbours model (Coords x y) =
    let
        neighbourCoords =
            [ Coords x (y - 1)
            , Coords (x + 1) (y - 1)
            , Coords (x + 1) y
            , Coords (x + 1) (y + 1)
            , Coords x (y + 1)
            , Coords (x - 1) (y + 1)
            , Coords (x - 1) y
            , Coords (x - 1) (y - 1)
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



-- Cell


type Cell
    = Dead
    | Alive


fromBool : Bool -> Cell
fromBool isAlive =
    if isAlive then
        Alive

    else
        Dead


flipCell : Cell -> Cell
flipCell cell =
    case cell of
        Dead ->
            Alive

        Alive ->
            Dead



-- Coordinates


type alias Index =
    Int


type Coords
    = Coords Int Int


toIndex : Coords -> Model -> Index
toIndex (Coords x y) model =
    y * model.width + x


toCoordinates : Model -> Index -> Coords
toCoordinates model i =
    Coords (modBy model.width i) (i // model.width)
