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


all : List Color
all =
    [ Red, Orange, Yellow, Green, Blue, Purple ]


toString : Color -> String
toString color =
    light color


light : Color -> String
light color =
    case color of
        Red ->
            "#eb4d4d"

        Orange ->
            "#ffb854"

        Yellow ->
            "#e7fa55"

        Green ->
            "#39e36c"

        Blue ->
            "#6ba1ff"

        Purple ->
            "#e268f2"

        White ->
            "white"


basic : Color -> String
basic color =
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
