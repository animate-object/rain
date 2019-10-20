module Triangle exposing (Triangle, draw, sizedTriangleGrid)

import Html as Html exposing (..)
import List exposing (..)
import Point exposing (..)
import Svg as Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Triangle =
    { a : Point.Point
    , b : Point.Point
    , c : Point.Point
    }


points : Triangle -> List Point.Point
points tri =
    [ tri.a, tri.b, tri.c ]



-------------------------------------------------------------------------------
-- Svg
-------------------------------------------------------------------------------


pointAttr : Triangle -> Svg.Attribute msg
pointAttr tri =
    points tri
        |> List.map Point.toString
        |> String.join " "
        |> Svg.Attributes.points


draw : Triangle -> List (Html.Attribute msg) -> List (Svg.Svg msg) -> Html msg
draw tri attrs children =
    polygon
        (List.append attrs [ pointAttr tri ])
        children



-------------------------------------------------------------------------------
-- 'Grid' of triangles
-------------------------------------------------------------------------------


sizedTriangleGrid : Int -> Float -> List (List Triangle)
sizedTriangleGrid rows boxWidth =
    let
        s =
            boxWidth / toFloat rows

        mid =
            boxWidth / 2

        thisRow n =
            nthRowOfTriangles n s mid
    in
    List.range 0 (rows - 1)
        |> List.map thisRow


nthRowOfTriangles : Int -> Float -> Float -> List Triangle
nthRowOfTriangles rowNum s mid =
    let
        count =
            rowNum * 2 + 1

        startX =
            mid - ((s / 2) * toFloat rowNum)

        startY =
            toFloat rowNum * s

        rowTriangle n =
            nthTriangleInRow
                n
                startX
                startY
                s
    in
    List.range 0 (count - 1)
        |> List.map rowTriangle


nthTriangleInRow : Int -> Float -> Float -> Float -> Triangle
nthTriangleInRow p x y s =
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
