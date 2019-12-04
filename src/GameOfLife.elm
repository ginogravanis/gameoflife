module GameOfLife exposing (main)

import Array
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import ListUtil
import Time
import World



-- CONSTANTS


worldWidth : Int
worldWidth =
    110


worldHeight : Int
worldHeight =
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
    { simulationState : SimulationState
    , world : World.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = World.init ( worldWidth, worldHeight )
      , simulationState = Stopped
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = StartButtonPressed
    | StopButtonPressed
    | Tick Time.Posix
    | GotWorldMsg World.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartButtonPressed ->
            ( { model | simulationState = Running }
            , Cmd.none
            )

        StopButtonPressed ->
            ( { model | simulationState = Stopped }
            , Cmd.none
            )

        Tick _ ->
            ( { model | world = World.evolve model.world }
            , Cmd.none
            )

        GotWorldMsg subMsg ->
            World.update subMsg model.world
                |> updateWith (Model model.simulationState) GotWorldMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
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


renderCell : World.Cell -> Html Msg
renderCell cell =
    let
        cellColor =
            case cell of
                World.Alive ->
                    "black"

                World.Dead ->
                    "#EEE"
    in
    Html.td
        [ Attr.style "padding" "0.25em"
        , Attr.style "background-color" cellColor
        ]
        []


renderRow : List World.Cell -> Html Msg
renderRow row =
    Html.tr [] (List.map renderCell row)


toTable : World.Model -> Html Msg
toTable world =
    Html.table
        [ Attr.style "font-family" "monospace"
        , Attr.style "margin" "auto"
        ]
        (world.cells
            |> Array.toList
            |> ListUtil.subdivideList world.width
            |> List.map renderRow
        )


view : Model -> Html Msg
view { world, simulationState } =
    Html.div []
        [ Html.h1 [ Attr.style "text-align" "center" ]
            [ Html.text "Game of Life" ]
        , Html.div
            [ Attr.style "margin" "1.5em"
            , Attr.style "text-align" "center"
            ]
            [ Html.button
                [ Attr.style "margin-right" "1ex"
                , Events.onClick (GotWorldMsg World.NewRequested)
                ]
                [ Html.text "New" ]
            , startStopButton simulationState
            ]
        , Html.div [ Attr.style "margin" "auto" ]
            [ toTable world ]
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
