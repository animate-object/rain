module Main exposing (main, view)

import Browser
import Color exposing (..)
import Graph exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Maybe exposing (..)
import Point exposing (..)
import Random exposing (..)
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Triangle exposing (..)
import Tuple exposing (..)


initRows =
    10


initSeed =
    23


main =
    Browser.sandbox
        { init = Model (Viewport 800 800) initRows (Tuple.first (adjacencyGraph initRows nodeData (initialSeed initSeed)))
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
    , color : Color
    }


type alias NodeDataGenerator =
    Seed -> Int -> ( NodeData, Seed )


nodeData : Seed -> Int -> ( NodeData, Seed )
nodeData seed id =
    let
        generated =
            Random.step Color.random seed
    in
    Tuple.pair (NodeData id (Tuple.first generated)) (Tuple.second generated)


type EdgeData
    = EdgeData


type alias TriGraph =
    Graph Int NodeData EdgeData


isEven : Int -> Bool
isEven int =
    remainderBy 2 int == 0


isOdd : Int -> Bool
isOdd int =
    not (isEven int)


adjacencyGraph : Int -> NodeDataGenerator -> Seed -> ( TriGraph, Seed )
adjacencyGraph rows generateNodeData seed =
    List.range 0 (rows - 1)
        |> List.foldl (addRow generateNodeData) (Tuple.pair Graph.empty seed)


addRow : NodeDataGenerator -> Int -> ( TriGraph, Seed ) -> ( TriGraph, Seed )
addRow generateNodeData r acc =
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
        |> List.foldl (addNode shouldConnect first r generateNodeData) acc


addNode : (Int -> Bool) -> Int -> Int -> NodeDataGenerator -> Int -> ( TriGraph, Seed ) -> ( TriGraph, Seed )
addNode shouldConnect first r nodeDataGenerator n acc =
    let
        generated =
            nodeDataGenerator (Tuple.second acc) n

        updated =
            Graph.insertData n (Tuple.first generated) (Tuple.first acc)

        withEdge =
            if n == first then
                updated

            else
                Graph.insertEdge n (n - 1) updated

        final =
            if shouldConnect n then
                Graph.insertEdge n (n - (2 * r)) withEdge

            else
                withEdge
    in
    Tuple.pair final (Tuple.second generated)



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
            (triangles
                model.rows
                viewWidth
                |> List.concat
                |> List.indexedMap (generateTri (generateAttributes model.graph))
            )
        , div
            []
            (debugGraph
                model.graph
            )
        ]


generateTri : (Int -> List (Html.Attribute msg)) -> Int -> Triangle -> Svg msg
generateTri generateAttr id tri =
    Triangle.draw tri (generateAttr id) []


generateAttributes : TriGraph -> Int -> List (Html.Attribute msg)
generateAttributes graph id =
    let
        maybeNode =
            Graph.getData id graph

        color =
            Maybe.withDefault White (Maybe.map (\n -> n.color) maybeNode)
    in
    [ fill (Color.toString color) ]


debugGraph : TriGraph -> List (Html msg)
debugGraph g =
    Graph.nodes g |> List.map (\n -> Html.div [] [ Html.text (nodeToString n g) ])


nodeToString : ( Int, Maybe NodeData ) -> TriGraph -> String
nodeToString node graph =
    "Color: "
        ++ Maybe.withDefault "None" (Tuple.second node |> Maybe.map (\data -> Color.toString data.color))
        ++ "Edges: "
        ++ (Graph.outgoing (Tuple.first node) graph |> Set.toList |> List.map (\num -> "-> " ++ String.fromInt num) |> String.join ", ")


edgesToString : List ( Int, Int ) -> String
edgesToString edges =
    List.map edgeToString edges |> String.join " || "


edgeToString : ( Int, Int ) -> String
edgeToString edge =
    String.fromInt (Tuple.first edge) ++ ", " ++ String.fromInt (Tuple.second edge)



-- 'Grid' of triangles


drawTriangles : Int -> Float -> List (List (Html.Attribute msg) -> List (Svg msg) -> Html msg)
drawTriangles rows boxWidth =
    let
        allTriangles =
            triangles rows boxWidth |> List.concat
    in
    List.map
        (\t -> Triangle.draw t)
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
