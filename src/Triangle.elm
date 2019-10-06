module Triangle exposing (Triangle, altitude, draw)

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


altitudeFromS : Float -> Float
altitudeFromS s =
    s * (sqrt 3 / 2)


altitude : Triangle -> Float
altitude eq =
    altitudeFromS (Point.distance eq.a eq.b)


draw : Triangle -> List (Html.Attribute msg) -> List (Svg.Svg msg) -> Html msg
draw tri attrs children =
    polygon
        (List.append attrs [ pointAttr tri ])
        children


pointAttr : Triangle -> Svg.Attribute msg
pointAttr tri =
    points tri
        |> List.map Point.toString
        |> String.join " "
        |> Svg.Attributes.points


points : Triangle -> List Point.Point
points tri =
    [ tri.a, tri.b, tri.c ]
