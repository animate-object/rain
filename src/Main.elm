module Main exposing (main, view)

import Browser
import Color exposing (..)
import FloodGraph exposing (FloodGraph, FloodNode, RecolorAccumulator, debugGraph, isFlooded, nodeColor, randomNode, recolor, triangleGraph)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (..)
import Point exposing (..)
import Random exposing (..)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Triangle exposing (Triangle, draw, sizedTriangleGrid)
import Tuple exposing (..)
import Viewport exposing (Viewport, trimViewport)


main =
    Browser.sandbox
        { init = init initRows (initialSeed initSeed) (Viewport 800 800)
        , update = update
        , view = view
        }



--------------------------------------------------------------------------------
-- Init
--------------------------------------------------------------------------------


initRows =
    10


initSeed =
    22122433


init : Int -> Seed -> Viewport -> Model
init rows seed viewport =
    let
        graph =
            triangleGraph initRows randomNode seed |> Tuple.first

        color =
            nodeColor graph 0

        gameData =
            GameData 0 False
    in
    Model viewport rows graph color gameData



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias GameData =
    { turnsTaken : Int
    , flooded : Bool
    }


type alias Model =
    { viewport : Viewport
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ColorSelected newColor ->
            let
                recoloredGraph =
                    recolor model.color newColor 0 (RecolorAccumulator model.graph Set.empty)
                        |> (\result -> result.graph)

                gameData =
                    model.gameData

                updatedGameData =
                    { gameData
                        | turnsTaken = gameData.turnsTaken + 1
                        , flooded = isFlooded recoloredGraph
                    }
            in
            { model
                | graph = recoloredGraph
                , color = newColor
                , gameData = updatedGameData
            }



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        trimmed =
            trimViewport model.viewport
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "display" "flex"
        , Attr.style "width" "100vw"
        , Attr.style "justify-content" "space-evenly"
        , Attr.style "height" "100vh"
        , Attr.style "font-family" "Arial, sans-serif"
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
                , Attr.style "justify-content" "space-evenly"
                , Attr.style "padding" "0.25rem"
                , Attr.style "flex-grow" "1"
                , Attr.style "align-items" "center"
                ]
                (colorSelectors
                    (\c -> Html.Events.onClick (ColorSelected c))
                )
            , gameInfo model.gameData
            ]
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
                    , Attr.style "box-shadow" "0 1px 3px rgba(0, 0, 0, 0.12), 0 1px 2px (0, 0, 0, 0.24)"
                    , Attr.style "padding" "1rem"
                    , Attr.style "background-color" (Color.toString c)
                    , onClick c
                    ]
                    []
            )



{------------------------------------------------------------------------------
-- This section contains a couple of functions that marry the graph in state
-- to the generated game grid (thereby generated the game board view).
-- Wondering if these fit better in some other module, but for now
-- it seems reasonable to do this where it's needed, in the view layer
--}


triangleGameGrid : Int -> FloodGraph -> Viewport -> List (Svg msg)
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
