module FloodGraph exposing (FloodGraph, FloodNode, RecolorAccumulator, debugGraph, empty, isFlooded, nodeColor, randomNode, recolor, triangleGraph)

import Color exposing (..)
import Graph exposing (..)
import Html exposing (Html, div)
import Random exposing (Seed)
import Set exposing (..)



-------------------------------------------------------------------------------
-- Graph
-------------------------------------------------------------------------------


type alias FloodNode =
    { id : Int
    , color : Color
    }


type alias FloodNodeGenerator =
    Seed -> Int -> ( FloodNode, Seed )


type EdgeData
    = EdgeData


type alias FloodGraph =
    Graph Int FloodNode EdgeData



-------------------------------------------------------------------------------
-- Generate
-------------------------------------------------------------------------------


empty : FloodGraph
empty =
    Graph.empty


randomNode : Seed -> Int -> ( FloodNode, Seed )
randomNode seed id =
    let
        generated =
            Random.step Color.random seed
    in
    Tuple.pair (FloodNode id (Tuple.first generated)) (Tuple.second generated)



-- Triangle Graph -------------------------------------------------------------


triangleGraph : Int -> FloodNodeGenerator -> Seed -> ( FloodGraph, Seed )
triangleGraph rows generateFloodNode seed =
    List.range 0 (rows - 1)
        |> List.foldl (addRow generateFloodNode) (Tuple.pair Graph.empty seed)


addRow : FloodNodeGenerator -> Int -> ( FloodGraph, Seed ) -> ( FloodGraph, Seed )
addRow generateFloodNode r acc =
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
        |> List.foldl (addNode shouldConnect first r generateFloodNode) acc


addNode : (Int -> Bool) -> Int -> Int -> FloodNodeGenerator -> Int -> ( FloodGraph, Seed ) -> ( FloodGraph, Seed )
addNode shouldConnect first r floodNodeGenerator n acc =
    let
        generated =
            floodNodeGenerator (Tuple.second acc) n

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



-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------


type alias RecolorAccumulator =
    { graph : FloodGraph
    , visited : Set Int
    }


recolor : Color -> Color -> Int -> RecolorAccumulator -> RecolorAccumulator
recolor current new nodeId acc =
    let
        maybeNode =
            Graph.getData nodeId acc.graph

        visited =
            Set.insert nodeId acc.visited

        unvisitedNeighbors =
            Set.diff (allEdges nodeId acc.graph) visited
    in
    Maybe.withDefault
        (RecolorAccumulator
            acc.graph
            visited
        )
        (Maybe.map
            (\node ->
                if node.color /= current then
                    RecolorAccumulator acc.graph visited

                else
                    let
                        recoloredNode =
                            FloodNode nodeId new
                    in
                    Set.foldl (recolor current new) (RecolorAccumulator (Graph.insertData nodeId recoloredNode acc.graph) visited) unvisitedNeighbors
            )
            maybeNode
        )



-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------


allEdges : comparable -> Graph comparable data edgeData -> Set comparable
allEdges id graph =
    Set.union (Graph.outgoing id graph) (Graph.incoming id graph)


nodeColor : FloodGraph -> Int -> Color
nodeColor graph id =
    Graph.getData id graph
        |> Maybe.map (\n -> n.color)
        |> Maybe.withDefault None


colorWithDefault : Maybe FloodNode -> Color
colorWithDefault node =
    Maybe.withDefault None (Maybe.map (\n -> n.color) node)


isFlooded : FloodGraph -> Bool
isFlooded graph =
    let
        allNodes =
            Graph.nodes graph |> List.map Tuple.second

        maybeFirstNodeData =
            List.head allNodes
    in
    case maybeFirstNodeData of
        Just (Just firstNodeData) ->
            List.all (\maybeData -> Maybe.withDefault False (Maybe.map (\n -> n.color == firstNodeData.color) maybeData)) allNodes

        _ ->
            False



-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------


debugGraph : FloodGraph -> List (Html msg)
debugGraph g =
    Graph.nodes g |> List.reverse |> List.map (\n -> Html.div [] [ Html.text (nodeToString n g) ])


nodeToString : ( Int, Maybe FloodNode ) -> FloodGraph -> String
nodeToString node graph =
    "Id: "
        ++ String.fromInt (Tuple.first node)
        ++ " || Color: "
        ++ Maybe.withDefault "None" (Tuple.second node |> Maybe.map (\data -> Color.toString data.color))
        ++ " || Edges: "
        ++ (allEdges (Tuple.first node) graph |> Set.toList |> List.map (\num -> "-> " ++ String.fromInt num) |> String.join ", ")


edgesToString : List ( Int, Int ) -> String
edgesToString edges =
    List.map edgeToString edges |> String.join " || "


edgeToString : ( Int, Int ) -> String
edgeToString edge =
    String.fromInt (Tuple.first edge) ++ ", " ++ String.fromInt (Tuple.second edge)



-------------------------------------------------------------------------------\
-- Misc
-------------------------------------------------------------------------------


isEven : Int -> Bool
isEven int =
    remainderBy 2 int == 0


isOdd : Int -> Bool
isOdd int =
    not (isEven int)
