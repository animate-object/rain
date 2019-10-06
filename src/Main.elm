module Main exposing (main, view)

import Browser
import Graph exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Maybe exposing (..)
import Point exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Triangle exposing (..)
import Tuple exposing (..)


initRows =
    32


main =
    Browser.sandbox
        { init = Model (Viewport 800 800) initRows (adjacencyGraph initRows)
        , update = \msg model -> model
        , view = view
        }


type alias Viewport =
    { height : Float
    , width : Float
    }


type alias Model =
    { viewport : Viewport
    , rows : Int
    , graph : TriGraph
    }



-- Graph


type alias NodeData =
    { id : Int
    }


type EdgeData
    = EdgeData


type alias GraphAcc graph =
    { graph : graph
    , step : Int
    }


type alias TriGraph =
    Graph Int NodeData EdgeData


type alias TriGraphAcc =
    GraphAcc TriGraph


isEven : Int -> Bool
isEven int =
    remainderBy 2 int == 0


isOdd : Int -> Bool
isOdd int =
    not (isEven int)


adjacencyGraph : Int -> TriGraph
adjacencyGraph rows =
    List.range 0 (rows - 1)
        |> List.foldl addRow Graph.empty


addRow : Int -> TriGraph -> TriGraph
addRow r g =
    let
        shouldConnect =
            if isEven r then
                isOdd

            else
                isEven

        first =
            r ^ 2

        last =
            ((r + 1) ^ 2) - 1
    in
    List.range first last
        |> List.foldl (addNode shouldConnect first r) g


addNode : (Int -> Bool) -> Int -> Int -> Int -> TriGraph -> TriGraph
addNode shouldConnect first r n g =
    let
        updated =
            Graph.insert n g

        withEdge =
            if n == first then
                g

            else
                Graph.insertEdge n (n - 1) g
    in
    if shouldConnect n then
        Graph.insertEdge n (n - (2 * r)) withEdge

    else
        withEdge



-- trim viewport to a square with sides equal to the minimum length of the viewport


trimViewport : Viewport -> Viewport
trimViewport v =
    let
        minSide =
            Basics.min v.height v.width
    in
    Viewport minSide minSide



-- View


view : Model -> Html msg
view model =
    let
        trimmed =
            trimViewport model.viewport

        viewHeight =
            trimmed.height

        viewWidth =
            trimmed.width
    in
    div []
        [ svg
            [ width (String.fromFloat viewWidth)
            , height (String.fromFloat viewHeight)
            ]
            (drawTriangles
                model.rows
                viewWidth
                [ stroke "blue"
                , fill "none"
                ]
            )
        , div
            []
            (debugGraph
                model.graph
            )
        ]


debugGraph : TriGraph -> List (Html msg)
debugGraph g =
    Graph.edges g |> List.map edgeToString |> List.map (\s -> Html.div [] [ Html.text s ])


edgeToString : ( Int, Int ) -> String
edgeToString edge =
    String.fromInt (Tuple.first edge) ++ ", " ++ String.fromInt (Tuple.second edge)



-- 'Grid' of triangles


drawTriangles : Int -> Float -> List (Html.Attribute msg) -> List (Svg msg)
drawTriangles rows boxWidth attrs =
    let
        allTriangles =
            triangles rows boxWidth |> List.concat
    in
    List.map
        (\t -> Triangle.draw t attrs [])
        allTriangles


triangles : Int -> Float -> List (List Triangle)
triangles rows boxWidth =
    let
        s =
            boxWidth / toFloat rows

        mid =
            boxWidth / 2

        thisRow n =
            trianglesRow n s mid
    in
    List.range 0 (rows - 1)
        |> List.map thisRow


trianglesRow : Int -> Float -> Float -> List Triangle
trianglesRow rowNum s mid =
    let
        count =
            rowNum * 2 + 1

        startX =
            mid - ((s / 2) * toFloat rowNum)

        startY =
            toFloat rowNum * s

        rowTriangle n =
            triangleN
                n
                startX
                startY
                s
    in
    List.range 0 (count - 1)
        |> List.map rowTriangle


triangleN : Int -> Float -> Float -> Float -> Triangle
triangleN p x y s =
    let
        offset =
            floor (toFloat p / 2)
    in
    if remainderBy 2 p == 0 then
        Triangle
            (Point (x + (toFloat offset * s)) y)
            (Point (x + (toFloat offset * s) - (s / 2)) (y + s))
            (Point (x + (toFloat offset * s) + (s / 2)) (y + s))

    else
        Triangle
            (Point (x + (toFloat offset * s)) y)
            (Point (x + (toFloat offset * s) + s) y)
            (Point (x + (toFloat offset * s) + (s / 2)) (y + s))
