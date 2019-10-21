module Color exposing (..)

import Random exposing (..)


type Color
    = Color1
    | Color2
    | Color3
    | Color4
    | Color5
    | Color6
    | EmptyColor


type alias Scheme =
    { name : String
    , fn : Color -> String
    }


classic =
    Scheme "Classic (blue)" rain


pastel =
    Scheme "Pastel" light


bold =
    Scheme "Bold" basic


schemes : List Scheme
schemes =
    [ classic
    , bold
    , pastel
    ]


all : List Color
all =
    [ Color1, Color2, Color3, Color4, Color5, Color6 ]


toString : Color -> Scheme -> String
toString color scheme =
    scheme.fn color


rain : Color -> String
rain color =
    case color of
        Color1 ->
            "#c9d0f5"

        Color2 ->
            "#35c7fc"

        Color3 ->
            "#424cff"

        Color4 ->
            "#23166e"

        Color5 ->
            "#96faf2"

        Color6 ->
            "#5f6968"

        EmptyColor ->
            "White"


light : Color -> String
light color =
    case color of
        Color1 ->
            "#eb4d4d"

        Color2 ->
            "#ffb854"

        Color3 ->
            "#e7fa55"

        Color4 ->
            "#39e36c"

        Color5 ->
            "#6ba1ff"

        Color6 ->
            "#e268f2"

        EmptyColor ->
            "White"


basic : Color -> String
basic color =
    case color of
        Color1 ->
            "Red"

        Color2 ->
            "Orange"

        Color3 ->
            "Yellow"

        Color4 ->
            "Blue"

        Color5 ->
            "Green"

        Color6 ->
            "Purple"

        EmptyColor ->
            "White"


random : Random.Generator Color
random =
    Random.uniform Color1 [ Color2, Color3, Color4, Color5, Color6 ]
