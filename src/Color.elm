module Color exposing (..)

import Random exposing (..)


type Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | White


toString : Color -> String
toString color =
    case color of
        Red ->
            "red"

        Orange ->
            "orange"

        Yellow ->
            "yellow"

        Green ->
            "green"

        Blue ->
            "blue"

        Purple ->
            "purple"

        White ->
            "white"


random : Random.Generator Color
random =
    Random.uniform Red [ Orange, Yellow, Green, Blue, Purple ]
