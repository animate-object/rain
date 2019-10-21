module Main exposing (main, view)

import Browser
import Browser.Dom
import Browser.Events
import Color exposing (..)
import FloodGraph exposing (FloodGraph, FloodNode, RecolorAccumulator, empty, isFlooded, nodeColor, randomNode, recolor, triangleGraph)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (..)
import Point exposing (..)
import Random exposing (..)
import Rect exposing (Rect, constrain, trim)
import Set exposing (..)
import Svg exposing (svg)
import Svg.Attributes as SvgAttr
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
    ( Model viewport
        rows
        FloodGraph.empty
        Color.EmptyColor
        (GameData 0 False)
        False
        (GameOptions
            (Just rows)
            Color.bold
        )
    , Cmd.batch [ requestNewGame, getViewport ]
    )



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias GameData =
    { turnsTaken : Int
    , flooded : Bool
    }


type alias GameOptions =
    { nextGameRows : Maybe Int
    , colorScheme : Color.Scheme
    }


type alias Model =
    { viewport : Rect
    , rows : Int
    , graph : FloodGraph
    , color : Color
    , gameData : GameData
    , showOptions : Bool
    , gameOptions : GameOptions
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
    | SetNextGameRows (Maybe Int)
    | SetColorScheme Color.Scheme


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
            ( { model | rows = Maybe.withDefault model.rows model.gameOptions.nextGameRows }, requestNewGame )

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
            let
                oldOptions =
                    model.gameOptions

                updatedGameOptions =
                    { oldOptions | nextGameRows = Just model.rows }
            in
            ( { model | showOptions = show, gameOptions = updatedGameOptions }, Cmd.none )

        SetNextGameRows val ->
            let
                oldOptions =
                    model.gameOptions

                updatedGameOptions =
                    { oldOptions
                        | nextGameRows =
                            Maybe.map (\v -> Basics.min 100 (Basics.max 0 v))
                                val
                    }
            in
            ( { model
                | gameOptions = updatedGameOptions
              }
            , Cmd.none
            )

        SetColorScheme scheme ->
            let
                oldOptions =
                    model.gameOptions

                updatedGameOptions =
                    { oldOptions
                        | colorScheme = scheme
                    }
            in
            ( { model
                | gameOptions = updatedGameOptions
              }
            , Cmd.none
            )



--------------------------------------------------------------------------------
-- Commands
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
        [ style "position" "absolute"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "top" "0"
        , style "overflow-x" "hidden"
        , style "font-family" "Arial, sans-serif"
        ]
        [ div
            [ style "display" "grid"
            , style "grid-template-rows" " 1em auto 1fr 1fr"
            , style "justify-items" "center"
            , style "grid-gap" "2em"
            , style "height" "100vh"
            ]
            [ div [] []
            , svg
                [ SvgAttr.width (String.fromFloat trimmed.width)
                , SvgAttr.height (String.fromFloat trimmed.height)
                ]
                (triangleGameGrid
                    model.gameOptions.colorScheme
                    model.rows
                    model.graph
                    trimmed
                )
            , div
                [ style "display" "grid"
                , style "grid-template-columns" "1fr 1fr 1fr"
                , style "grid-template-rows" "1fr 1fr"
                , style "grid-gap" "1em"
                , style "width" "90%"
                ]
                (colorSelectors model.gameOptions.colorScheme
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
        [ style "position" "absolute"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "top" "0"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "overflow" "none"
        , style "overflow" "none"
        , style "font-family" "Arial, sans-serif"
        , style "padding-top" "2%"
        ]
        [ div
            [ style "height" "100%"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            [ svg
                [ SvgAttr.width (String.fromFloat trimmed.width)
                , SvgAttr.height (String.fromFloat trimmed.height)
                ]
                (triangleGameGrid
                    model.gameOptions.colorScheme
                    model.rows
                    model.graph
                    trimmed
                )
            , div
                [ style "padding-top" "2rem"
                , style "display" "grid"
                , style "width" "100%"
                , style "grid-template-columns" "1fr 1fr 1fr 1fr 1fr 1fr"
                , style "grid-gap" "2rem"
                , style "flex-grow" "1"
                ]
                (colorSelectors model.gameOptions.colorScheme
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
        [ style "display" "grid"
        , style "width" "100%"
        , style "font-size" "3rem"
        , style "flex-grow" "1"
        , style "place-items" "center"
        , style "grid-template-columns" "0.5fr 0.5fr"
        , style "grid-gap" "1.5rem"
        ]
        [ Html.text
            (if flooded then
                "You won in " ++ String.fromInt turnsTaken ++ " turns!"

             else
                "Turn " ++ String.fromInt (turnsTaken + 1)
            )
        , button
            [ onClick (ShowOptions (not model.showOptions))
            , style "font-size" "3rem"
            , style "border" "none"
            , style "border-radius" "2px"
            , style "height" "50%"
            ]
            [ Html.text "Options" ]
        ]


colorSelectors : Color.Scheme -> (Color -> Html.Attribute msg) -> List (Html msg)
colorSelectors scheme onClick =
    Color.all
        |> List.map
            (\c ->
                button
                    [ style "height"
                        "70%"
                    , style
                        "flex-grow"
                        "1"
                    , style "border" "none"
                    , style "border-radius" "2px"
                    , style "padding" "1rem"
                    , style "background-color" (Color.toString c scheme)
                    , onClick c
                    ]
                    []
            )


desktopModalAttrs : List (Html.Attribute Msg)
desktopModalAttrs =
    [ style "position" "absolute"
    , style "z-index" "100"
    , style "top" "1%"
    , style "height" "98%"
    , style "width" "80%"
    , style "border-radius" "6px"
    , style "background-color" "#efefef"
    ]


mobileModalAttrs : List (Html.Attribute Msg)
mobileModalAttrs =
    [ style "position" "absolute"
    , style "top" "1%"
    , style "left" "1%"
    , style "z-index" "100"
    , style "height" "98%"
    , style "width" "98%"
    , style "border-radius" "6px"
    , style "background-color" "#efefef"
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
        [ style "height" "100%"
        , style "display" "grid"
        , style "grid-gap" "1em"
        , style "grid-template-rows" "1fr auto 1fr 1fr 1fr"
        , style "justify-items" "center"
        , style "padding" "1rem"
        ]
        [ Html.h1 [] [ Html.text "Rain" ]
        , Html.p [ style "font-size" "3rem" ]
            [ Html.text
                ("Rain is a triangular take on a 'color the grid game'."
                    ++ " Change the color of the top node to expand your selection."
                    ++ " Try to cover the whole grid!"
                )
            ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "1fr 1fr"
            , style "justify-content" "start"
            , style "font-size" "3rem"
            , style "grid-gap" "0.5rem"
            ]
            [ label [] [ text "Set Color Scheme" ]
            , selectColorScheme model.gameOptions.colorScheme
            , label [] [ text "Set #Rows (requires new game)" ]
            , setNextGameRows model.gameOptions.nextGameRows
            ]
        , button
            [ onClick NewGameRequested
            , style "font-size" "3rem"
            , style "border" "none"
            , style "border-radius" "2px"
            , style "height" "80%"
            , style "width" "100%"
            ]
            [ Html.text "New Game" ]
        , button
            [ onClick (ShowOptions False)
            , style "font-size" "3rem"
            , style "border" "none"
            , style "border-radius" "2px"
            , style "height" "80%"
            , style "width" "100%"
            ]
            [ Html.text "Exit" ]
        ]


setNextGameRows : Maybe Int -> Html Msg
setNextGameRows nextGameRows =
    input
        [ type_ "number"
        , style "font-size" "3rem"
        , value (Maybe.map String.fromInt nextGameRows |> withDefault "")
        , onInput (\s -> SetNextGameRows (String.toInt s))
        ]
        []


selectColorScheme : Color.Scheme -> Html Msg
selectColorScheme selectedScheme =
    select
        [ onInput
            (\val ->
                List.filter (\scheme -> scheme.name == val) Color.schemes
                    |> List.head
                    |> Maybe.map (\scheme -> SetColorScheme scheme)
                    |> Maybe.withDefault (SetColorScheme selectedScheme)
            )
        , style "font-size" "3rem"
        ]
        (Color.schemes
            |> List.map
                (\scheme ->
                    option
                        [ value scheme.name
                        , style "font-size" "2rem"
                        , selected (scheme.name == selectedScheme.name)
                        ]
                        [ text scheme.name ]
                )
        )



{------------------------------------------------------------------------------
-- This section contains a couple of functions that marry the graph in state
-- to the generated game grid (thereby generated the game board view).
-- Wondering if these fit better in some other module, but for now
-- it seems reasonable to do this where it's needed, in the view layer
--}


triangleGameGrid : Color.Scheme -> Int -> FloodGraph -> Rect -> List (Svg.Svg msg)
triangleGameGrid scheme rowCount graph viewport =
    sizedTriangleGrid
        rowCount
        viewport.width
        |> List.concat
        |> List.indexedMap (coloredTriangle (nodeColorFromId scheme graph))


coloredTriangle : (Int -> List (Svg.Attribute msg)) -> Int -> Triangle -> Svg.Svg msg
coloredTriangle getColorAttr nodeId tri =
    Triangle.draw tri (getColorAttr nodeId) []


nodeColorFromId : Color.Scheme -> FloodGraph -> Int -> List (Svg.Attribute msg)
nodeColorFromId scheme graph id =
    nodeColor graph id
        |> (\color -> [ SvgAttr.fill (Color.toString color scheme) ])
