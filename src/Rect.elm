module Rect exposing (Rect, constrain, trim)


type alias Rect =
    { width : Float
    , height : Float
    }


trim : Rect -> Rect
trim v =
    let
        minSide =
            Basics.min v.height v.width
    in
    Rect minSide minSide


constrain : Rect -> Rect -> Rect
constrain target constraint =
    Rect (min target.width constraint.width) (min target.height constraint.height)
