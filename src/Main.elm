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
    ( Model viewport rows FloodGraph.empty Color.White (GameData 0 False) False rows, Cmd.batch [ requestNewGame, getViewport ] )



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
    , showOptions : Bool
    , nextGameRows : Int
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = ColorSelected Color
    | NewGameStarted Seed
    | NewGameRequested
    | WindowResized Int Int
    | ViewportUpdated Rect
    | ShowOptions Bool
    | SetNextGameRows Int


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
            if newColor == model.color then
                ( model, Cmd.none )

            else
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
            ( { model | rows = model.nextGameRows }, requestNewGame )

        NewGameStarted seed ->
            let
                game =
                    newGame model.rows seed

                graph =
                    Tuple.first game
            in
            ( { model
                | graph = graph
                , color = nodeColor graph startNode
                , gameData = Tuple.second game
                , showOptions = False
              }
            , Cmd.none
            )

        ViewportUpdated newViewport ->
            ( { model | viewport = newViewport }, Cmd.none )

        WindowResized _ _ ->
            ( model, getViewport )

        ShowOptions show ->
            -- reset options state on 'enter/exit'
            ( { model | showOptions = show, nextGameRows = model.rows }, Cmd.none )

        SetNextGameRows rows ->
            ( { model | nextGameRows = rows }, Cmd.none )



--------------------------------------------------------------------------------
-- Commanda
--------------------------------------------------------------------------------


getViewport : Cmd Msg
getViewport =
    Task.perform (\v -> ViewportUpdated (Rect v.scene.width v.scene.height)) Browser.Dom.getViewport


requestNewGame : Cmd Msg
requestNewGame =
    Task.perform (\posix -> Time.posixToMillis posix |> initialSeed |> NewGameStarted) Time.now



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
    if model.viewport.width > 982 then
        desktop model

    else
        mobile model


mobile : Model -> Html Msg
mobile model =
    let
        constrained =
            Rect.constrain model.viewport (Rect (model.viewport.width * 0.9) (model.viewport.height * 0.65))

        trimmed =
            Rect.trim constrained
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "top" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "font-family" "Arial, sans-serif"
        ]
        [ div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-rows" "1fr auto 1.5fr 1fr"
            , Attr.style "justify-items" "center"
            , Attr.style "grid-gap" "2em"
            , Attr.style "height" "100vh"
            ]
            [ Html.h1 [ Attr.style "font-size" "5rem", Attr.style "text-align" "center" ] [ Html.text "Rain" ]
            , svg
                [ Svg.Attributes.width (String.fromFloat trimmed.width)
                , Svg.Attributes.height (String.fromFloat trimmed.height)
                ]
                (triangleGameGrid
                    model.rows
                    model.graph
                    trimmed
                )
            , div
                [ Attr.style "display" "grid"
                , Attr.style "grid-template-columns" "1fr 1fr 1fr"
                , Attr.style "grid-template-rows" "1fr 1fr"
                , Attr.style "grid-gap" "1em"
                , Attr.style "width" "90%"
                ]
                (colorSelectors
                    (\c -> Html.Events.onClick (ColorSelected c))
                )
            , gamePanel model
            ]
        , gameOptionsModal model
        ]


desktop : Model -> Html Msg
desktop model =
    let
        constrained =
            Rect model.viewport.width (0.75 * model.viewport.height)

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
                [ Attr.style "padding-top" "2rem"
                , Attr.style "display" "grid"
                , Attr.style "width" "100%"
                , Attr.style "grid-template-columns" "1fr 1fr 1fr 1fr 1fr 1fr"
                , Attr.style "grid-gap" "2rem"
                , Attr.style "flex-grow" "1"
                ]
                (colorSelectors
                    (\c -> Html.Events.onClick (ColorSelected c))
                )
            , gamePanel model
            ]
        , gameOptionsModal model
        ]


gamePanel : Model -> Html Msg
gamePanel model =
    let
        turnsTaken =
            model.gameData.turnsTaken

        flooded =
            model.gameData.flooded
    in
    div
        [ Attr.style "display" "grid"
        , Attr.style "width" "100%"
        , Attr.style "font-size" "3rem"
        , Attr.style "flex-grow" "1"
        , Attr.style "place-items" "center"
        , Attr.style "grid-template-columns" "1fr 1fr"
        , Attr.style "grid-gap" "1.5rem"
        ]
        [ Html.text
            (if flooded then
                "You won in " ++ String.fromInt turnsTaken ++ "turns!"

             else
                "Turn " ++ String.fromInt (turnsTaken + 1)
            )
        , button
            [ onClick (ShowOptions (not model.showOptions))
            , Attr.style "font-size" "3rem"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "2px"
            , Attr.style "height" "50%"
            ]
            [ Html.text "Show Options" ]
        ]


colorSelectors : (Color -> Html.Attribute msg) -> List (Html msg)
colorSelectors onClick =
    Color.all
        |> List.map
            (\c ->
                button
                    [ Attr.style "height"
                        "70%"
                    , Attr.style
                        "flex-grow"
                        "1"
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
    , Attr.style "top" "1%"
    , Attr.style "height" "98%"
    , Attr.style "width" "50%"
    , Attr.style "border-radius" "6px"
    , Attr.style "background-color" "#efefef"
    ]


mobileModalAttrs : List (Html.Attribute Msg)
mobileModalAttrs =
    [ Attr.style "position" "absolute"
    , Attr.style "top" "1%"
    , Attr.style "left" "1%"
    , Attr.style "z-index" "100"
    , Attr.style "height" "98%"
    , Attr.style "width" "98%"
    , Attr.style "border-radius" "6px"
    , Attr.style "background-color" "#efefef"
    ]


modal : Bool -> Rect -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
modal isVisible viewport attrs children =
    let
        modalAttrs =
            if viewport.width < 982 then
                mobileModalAttrs

            else
                desktopModalAttrs
    in
    if isVisible == True then
        div (List.append modalAttrs attrs) children

    else
        div [] []


gameOptionsModal : Model -> Html Msg
gameOptionsModal model =
    modal model.showOptions
        model.viewport
        []
        [ gameOptions model ]


gameOptions : Model -> Html Msg
gameOptions model =
    div
        [ Attr.style "height" "100%"
        , Attr.style "display" "grid"
        , Attr.style "grid-gap" "1em"
        , Attr.style "grid-template-rows" "1fr auto 1fr 1fr 1fr"
        , Attr.style "justify-items" "center"
        , Attr.style "padding" "1rem"
        ]
        [ Html.h1 [] [ Html.text "Rain" ]
        , Html.p [ Attr.style "font-size" "3rem" ]
            [ Html.text
                ("Rain is a triangular take on a 'color the grid game'."
                    ++ " Change the color of the top node to expand your selection."
                    ++ " Try to cover the whole grid!"
                )
            ]
        , div
            [ Attr.style "font-size" "3rem"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "2px"
            , Attr.style "padding-top" "15%"
            , Attr.style "height" "80%"
            , Attr.style "width" "100%"
            ]
            [ Html.span [ Attr.style "font-size" "3rem" ]
                [ Html.text "Number of rows" ]
            , Html.input
                [ Attr.type_ "number"
                , Attr.style "font-size" "3rem"
                , Html.Events.onInput (\s -> SetNextGameRows (Maybe.withDefault model.nextGameRows (String.toInt s)))
                , Attr.value (String.fromInt model.nextGameRows)
                ]
                []
            ]
        , button
            [ onClick NewGameRequested
            , Attr.style "font-size" "3rem"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "2px"
            , Attr.style "height" "80%"
            , Attr.style "width" "100%"
            ]
            [ Html.text "New Game" ]
        , button
            [ onClick (ShowOptions False)
            , Attr.style "font-size" "3rem"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "2px"
            , Attr.style "height" "80%"
            , Attr.style "width" "100%"
            ]
            [ Html.text "Exit" ]
        ]



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
