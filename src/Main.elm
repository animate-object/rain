module Main exposing (main, view)

import Browser
import Browser.Dom
import Browser.Events
import Color exposing (..)
import FloodGraph exposing (FloodGraph, FloodNode, RecolorAccumulator, debugGraph, empty, isFlooded, nodeColor, randomNode, recolor, triangleGraph)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (..)
import Point exposing (..)
import Random exposing (..)
import Rect exposing (Rect, constrain, trim)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (..)
import Triangle exposing (Triangle, draw, sizedTriangleGrid)
import Tuple exposing (..)


main =
    Browser.element
        { init = \flags -> init flags initRows (initialSeed 0) (Rect 800 800)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



--------------------------------------------------------------------------------
-- Init
--------------------------------------------------------------------------------


initRows =
    10


startNode =
    0


init : () -> Int -> Seed -> Rect -> ( Model, Cmd Msg )
init flags rows seed viewport =
    ( Model viewport rows FloodGraph.empty Color.White (GameData 0 False), Cmd.batch [ requestNewGame, getInitialViewport ] )



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias GameData =
    { turnsTaken : Int
    , flooded : Bool
    }


type alias Model =
    { viewport : Rect
    , rows : Int
    , graph : FloodGraph
    , color : Color
    , gameData : GameData
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = ColorSelected Color
    | NewGameStarted Seed
    | NewGameRequested
    | WindowResized Int Int


getInitialViewport : Cmd Msg
getInitialViewport =
    Task.perform (\v -> WindowResized (floor v.viewport.width) (floor v.viewport.height)) Browser.Dom.getViewport


requestNewGame : Cmd Msg
requestNewGame =
    Task.perform (\posix -> Time.posixToMillis posix |> initialSeed |> NewGameStarted) Time.now


newGame : Int -> Seed -> ( FloodGraph, GameData )
newGame rows seed =
    let
        graph =
            triangleGraph rows randomNode seed |> Tuple.first

        gameData =
            GameData 0 False
    in
    ( graph, gameData )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ColorSelected newColor ->
            let
                recoloredGraph =
                    recolor model.color newColor startNode (RecolorAccumulator model.graph Set.empty)
                        |> (\result -> result.graph)

                gameData =
                    model.gameData

                updatedGameData =
                    { gameData
                        | turnsTaken = gameData.turnsTaken + 1
                        , flooded = isFlooded recoloredGraph
                    }

                updated =
                    { model
                        | graph = recoloredGraph
                        , color = newColor
                        , gameData = updatedGameData
                    }
            in
            ( updated, Cmd.none )

        NewGameRequested ->
            ( model, requestNewGame )

        NewGameStarted seed ->
            let
                game =
                    newGame model.rows seed

                graph =
                    Tuple.first game
            in
            ( Model model.viewport model.rows graph (nodeColor graph startNode) (Tuple.second game), Cmd.none )

        WindowResized width height ->
            ( { model | viewport = Rect (toFloat width) (toFloat height) }, Cmd.none )



--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResized



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    if model.viewport.width > 750 then
        desktop model

    else
        mobile model


mobile : Model -> Html Msg
mobile model =
    div [] []


desktop : Model -> Html Msg
desktop model =
    let
        constrained =
            Rect model.viewport.width (0.75 * model.viewport.height)

        gutter =
            Rect model.viewport.width (0.2 * model.viewport.height)

        trimmed =
            Rect.trim constrained
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "top" "0"
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "overflow" "none"
        , Attr.style "overflow" "none"
        , Attr.style "font-family" "Arial, sans-serif"
        , Attr.style "padding-top" "2%"
        ]
        [ div
            [ Attr.style "height" "100%"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            ]
            [ svg
                [ Svg.Attributes.width (String.fromFloat trimmed.width)
                , Svg.Attributes.height (String.fromFloat trimmed.height)
                ]
                (triangleGameGrid
                    model.rows
                    model.graph
                    trimmed
                )
            , div
                [ Attr.style "display" "flex"
                , Attr.style "width" "100%"
                , Attr.style "justify-content" "space-between"
                , Attr.style "flex-grow" "1"
                , Attr.style "align-items" "center"
                ]
                (colorSelectors
                    (\c -> Html.Events.onClick (ColorSelected c))
                )
            , gameInfo model.gameData
            ]
        , modal
            False
            model.viewport
            []
            [ div [] [ Html.text "test" ] ]
        ]


gameInfo : GameData -> Html msg
gameInfo gameData =
    div
        [ Attr.style "display" "flex"
        , Attr.style "width" "100%"
        , Attr.style "justify-content" "space-evenly"
        , Attr.style "padding" "0.25rem"
        , Attr.style "font-size" "1.5rem"
        , Attr.style "flex-grow" "1"
        , Attr.style "align-items" "center"
        ]
        [ Html.text
            ("Turns Taken "
                ++ String.fromInt gameData.turnsTaken
                ++ (if gameData.flooded then
                        " You've Won!"

                    else
                        ""
                   )
            )
        ]


colorSelectors : (Color -> Html.Attribute msg) -> List (Html msg)
colorSelectors onClick =
    Color.all
        |> List.map
            (\c ->
                button
                    [ Attr.style "width" "5rem"
                    , Attr.style "height" "3rem"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "2px"
                    , Attr.style "padding" "1rem"
                    , Attr.style "background-color" (Color.toString c)
                    , onClick c
                    ]
                    []
            )


desktopModalAttrs : List (Html.Attribute Msg)
desktopModalAttrs =
    [ Attr.style "position" "absolute"
    , Attr.style "z-index" "100"
    , Attr.style "top" "10%"
    , Attr.style "height" "80%"
    , Attr.style "width" "50%"
    , Attr.style "border-radius" "4px"
    , Attr.style "border" "1px solid #e5e5e5"
    , Attr.style "background-color" "#efefef"
    ]


mobileModalAttrs : List (Html.Attribute Msg)
mobileModalAttrs =
    [ Attr.style "position" "absolute"
    , Attr.style "top" "5%"
    , Attr.style "z-index" "100"
    , Attr.style "height" "90%"
    , Attr.style "width" "90%"
    , Attr.style "background-color" "#e5e5e5"
    ]


modal : Bool -> Rect -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
modal isVisible viewport attrs children =
    let
        modalAttrs =
            if viewport.width < 800 then
                mobileModalAttrs

            else
                desktopModalAttrs
    in
    if isVisible == True then
        div (List.append modalAttrs attrs) children

    else
        div [] []



{------------------------------------------------------------------------------
-- This section contains a couple of functions that marry the graph in state
-- to the generated game grid (thereby generated the game board view).
-- Wondering if these fit better in some other module, but for now
-- it seems reasonable to do this where it's needed, in the view layer
--}


triangleGameGrid : Int -> FloodGraph -> Rect -> List (Svg msg)
triangleGameGrid rowCount graph viewport =
    sizedTriangleGrid
        rowCount
        viewport.width
        |> List.concat
        |> List.indexedMap (coloredTriangle (nodeColorFromId graph))


coloredTriangle : (Int -> List (Svg.Attribute msg)) -> Int -> Triangle -> Svg msg
coloredTriangle getColorAttr nodeId tri =
    Triangle.draw tri (getColorAttr nodeId) []


nodeColorFromId : FloodGraph -> Int -> List (Svg.Attribute msg)
nodeColorFromId graph id =
    nodeColor graph id
        |> (\color -> [ fill (Color.toString color) ])
